unit lpcompilerDebug;

interface

uses System.Generics.Collections, System.SysUtils, System.Classes,
     lpvartypes_array, lpcompiler, lpvartypes, lptypes, lptree,
     lpparser, lpmessages;

type
  TVarOffset = record
    Name : String;
    StackIndex : Integer;
    Offset : Integer;
    StackVar: TLapeStackVar;
    Size : Integer;
    Address : Pointer;
  end;
  TVarsList = TArray<TVarOffset>;

  TMethodDescr = record
     Name : String;
     ParamsList : TVarsList;
     isFunction : Boolean;
     Result : TVarOffset;
     VarsList : TVarsList;
     AllObjList : TVarsList;

  end;
  PMethodDescr = ^TMethodDescr;

  TLapeCompilerDebug = class(TLapeCompiler)
  private
    fScriptFileName : String;
  protected
    CallStackNamesList : TList<String>;
    fCallStack : TStringList;

    function LapeMethodCreated(Sender: TLapeCompilerBase; MethodInfo: TLapeType_Method; MethodEndPos: TDocPos) : TMethodDescr;
    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method; override;
//    function HandleDuplicate(const FuncName : String; var NewName : String; DocPos: TDocPos) : Boolean; override;
//    function HandleVarDuplicate(VarName : String; var NewName : String; DocPos: TDocPos) : Boolean; override;

  public
    MethodsList : TDictionary<String,TMethodDescr>;
    FunctionList : TList<String>;


    constructor Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    ); override;
    destructor Destroy; override;

    function CallStack : TStringList;
    function Compile: Boolean; override;
    function CurrentMethodParams : TArray<TVarOffset>;
    function CurrentMethodHasResult : Boolean;
    function CurrentMethodResult : TVarOffset;
    function CurrentMethodVars : TArray<TVarOffset>;
    function GlobalVars : TArray<TLapeGlobalVar>;
    property ScriptFileName : String read fScriptFileName;
  end;


implementation

{$Region 'MethodsParsing'}
procedure Lape_EnterMethod(const Params: PParamArray);
var
  Compiler: TLapeCompilerDebug;
begin
  Compiler := TLapeCompilerDebug(Params^[0]);
  Compiler.CallStackNamesList.Add(PlpString(Params^[1])^);
end;

procedure Lape_LeaveMethod(const Params: PParamArray);
var
  Compiler: TLapeCompilerDebug;
  MethodName : String;
  ListIndex : Integer;
begin
  Compiler := TLapeCompilerDebug(Params^[0]);
  MethodName := PlpString(Params^[1])^;
  if MethodName <> '' then //if '' - root maybe
  begin
    ListIndex := Compiler.CallStackNamesList.IndexOf(MethodName);
    if (ListIndex = -1) or (ListIndex < Compiler.CallStackNamesList.Count - 1) then
      LapeException('LeaveMethod: Method not in Call Stack or not on top!');
    Compiler.CallStackNamesList.Remove(MethodName);
  end;

end;

procedure Lape_ValuesMethodHeader(const Params: PParamArray);
var
  MethodDescr : TMethodDescr;
  StealthLapeCompiler : TLapeCompilerDebug;
  i,k : Integer;
  CurrName : String;
  IsFound : Boolean;
begin
  StealthLapeCompiler := TLapeCompilerDebug(Params^[0]);
  if not StealthLapeCompiler.MethodsList.ContainsKey(PlpString(Params^[1])^) then
    Exit;
  MethodDescr := StealthLapeCompiler.MethodsList.Items[PlpString(Params^[1])^];
  for I := 0 to Length(MethodDescr.AllObjList)-1 do
    MethodDescr.AllObjList[i].Address := Pointer(Params^[i+2]);

  for I := 0 to Length(MethodDescr.AllObjList)-1 do
  begin
    CurrName := MethodDescr.AllObjList[i].Name;
    IsFound := False;
    for K := 0 to Length(MethodDescr.ParamsList)-1 do
      if MethodDescr.ParamsList[k].Name = CurrName then
      begin
        MethodDescr.ParamsList[k].Address := MethodDescr.AllObjList[i].Address;
        IsFound := True;
        Break;
      end;
    if IsFound then
      Continue;
    for K := 0 to Length(MethodDescr.VarsList)-1 do
      if MethodDescr.VarsList[k].Name = CurrName then
      begin
        MethodDescr.VarsList[k].Address := MethodDescr.AllObjList[i].Address;
        IsFound := True;
        Break;
      end;
    if IsFound then
      Continue;
    if MethodDescr.isFunction then
     MethodDescr.Result.Address := MethodDescr.AllObjList[i].Address;
  end;
  StealthLapeCompiler.MethodsList.Items[PlpString(Params^[1])^] := MethodDescr;
end;


function TLapeCompilerDebug.Compile: Boolean;
begin
  addGlobalMethod('procedure _EnterMethod(constref Name: String);', @Lape_EnterMethod, Self);
  addGlobalMethod('procedure _LeaveMethod(constref Name: String; Exception: Boolean);', @Lape_LeaveMethod, Self);
  Result := inherited Compile();
end;

function TLapeCompilerDebug.LapeMethodCreated(Sender: TLapeCompilerBase; MethodInfo: TLapeType_Method; MethodEndPos: TDocPos) : TMethodDescr;
var VarOffset : TVarOffset;
    i,k  : Integer;
    MethodDescr : TMethodDescr;
begin
  MethodDescr.Name := MethodInfo.Name;

  if MethodInfo.Params.Count > 0 then
    for i := 0 to MethodInfo.Params.Count - 1 do
    begin
      VarOffset.StackIndex := i;
      VarOffset.Offset := Sender.StackInfo.Vars[i].Offset;
      VarOffset.Name := Sender.StackInfo.Vars[i].Name;
      VarOffset.Size := Sender.StackInfo.Vars[i].Size;
      VarOffset.StackVar := Sender.StackInfo.Vars[i];
      VarOffset.Address := nil;
      MethodDescr.ParamsList := MethodDescr.ParamsList + [VarOffset];
      MethodDescr.AllObjList := MethodDescr.AllObjList + [VarOffset];
    end;
  i := MethodInfo.Params.Count;
  MethodDescr.isFunction := Assigned(MethodInfo.Res);
  if MethodDescr.isFunction then
  begin
    MethodDescr.Result.StackIndex := i;
    MethodDescr.Result.Offset := Sender.StackInfo.Vars[i].Offset;
    MethodDescr.Result.Name := Sender.StackInfo.Vars[i].Name;
    MethodDescr.Result.Size := Sender.StackInfo.Vars[i].Size;
    MethodDescr.Result.StackVar := Sender.StackInfo.Vars[i];
    MethodDescr.Result.Address := nil;
    MethodDescr.AllObjList := MethodDescr.AllObjList + [MethodDescr.Result];
    Inc(i);
  end;
  if Sender.StackInfo.VarStack.Count > i then
    for k := i to Sender.StackInfo.VarStack.Count - 1 do
    begin
      //тут перебираем оставшееся, это переменные метода
      VarOffset.StackIndex := k;
      VarOffset.Offset := Sender.StackInfo.Vars[k].Offset;
      VarOffset.Name := Sender.StackInfo.Vars[k].Name;
      VarOffset.Size := Sender.StackInfo.Vars[k].Size;
      VarOffset.StackVar := Sender.StackInfo.Vars[k];
      VarOffset.Address := nil;
      MethodDescr.VarsList := MethodDescr.VarsList + [VarOffset];
      MethodDescr.AllObjList := MethodDescr.AllObjList + [VarOffset];
    end;

  MethodsList.Add(MethodInfo.Name, MethodDescr);
  Result := MethodDescr;
  if Assigned(FunctionList) then //CreateForParse
    FunctionList.Add(MethodInfo.Name);
end;

//function TLapeCompilerDebug.HandleDuplicate(const FuncName : String; var NewName : String; DocPos: TDocPos) : Boolean;
//begin
//  Hint('Detected overrided Stealth method "'+FuncName+'". Its bad practice,'
//      +' please change the method name!',[],DocPos);
//  NewName := FuncName + FDuplicates.Count.ToString;
//  Result := True;
//end;
//
//function TLapeCompilerDebug.HandleVarDuplicate(VarName : String; var NewName : String; DocPos: TDocPos) : Boolean;
//begin
//  if not FDuplicates.ContainsKey(VarName) then
//    FDuplicates.Add(VarName, VarName + FDuplicates.Count.ToString);
//  Hint('Detected overrided Stealth method "'+VarName+'". Its bad practice,'
//      +' please change the var name!',[],DocPos);
//  NewName := VarName + FDuplicates.Count.ToString;
//  Result := True;
//end;

function TLapeCompilerDebug.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method;
var
  EnterMethod, LeaveMethod,ValuesMethod: TLapeTree_Invoke;
  Statement: TLapeTree_Try;
  Test: TLapeTree_Operator;
  MethodDescr : TMethodDescr;
  ValuesMethodHeader : String;
  i : Integer;
  Decl: TLapeDeclaration;
  ExprBase : TLapeTree_ExprBase;
  AllExprAdded : Boolean;
  VarName, VarType : String;
  DocPos : TDocPos;
begin
  Result := inherited ParseMethod(FuncForwards, FuncHeader, FuncName, isExternal);

  if (Result <> nil) and (Result.Statements <> nil) then
  begin
//    FuncName := UpperCase(FuncName);
    if (FuncName = '') or (FuncName[1] = '!')
       or (FuncName = '_ENTERMETHOD') or (FuncName = '_LEAVEMETHOD')
       or (FuncHeader is TLapeType_MethodOfType) then
      Exit;

    if (Result.DocPos.FileName <> '') and (not Result.DocPos.FileName.StartsWith('!'))
      and (Result.Method.Name <> '') then

    if hasDefine('DEBUGGING') then
    begin
      MethodDescr := LapeMethodCreated(Self,
        TLapeType_Method(Result.Method.VarType),Tokenizer.DocPos);

      ValuesMethodHeader := 'procedure Vals_' + FuncName + '(FuncName : String;';
      if Length(MethodDescr.AllObjList) > 0 then
      begin
        for i := 0 to Length(MethodDescr.AllObjList) - 1 do
        begin
//            Decl := getDeclaration(MethodDescr.AllObjList[i].StackVar.Name,Result.StackInfo, true);
          if MethodDescr.AllObjList[i].StackVar.Name = 'Result' then
            VarName := 'Res'
          else
            VarName := MethodDescr.AllObjList[i].StackVar.Name;

          if MethodDescr.AllObjList[i].StackVar.VarType.Name = '' then
          begin //масивы
            case MethodDescr.AllObjList[i].StackVar.VarType.BaseType of
              ltDynArray,ltStaticArray :
              begin
                VarType :='array of '+ TLapeType_DynArray(MethodDescr.AllObjList[i].StackVar.VarType).PType.Name;
              end;
              else
                VarType := MethodDescr.AllObjList[i].StackVar.VarType.Name;
            end;
          end
          else
            VarType := MethodDescr.AllObjList[i].StackVar.VarType.Name;

          ValuesMethodHeader := ValuesMethodHeader+VarName+':'+VarType+';';
        end;

        Delete(ValuesMethodHeader,Length(ValuesMethodHeader),1);
        ValuesMethodHeader := ValuesMethodHeader + ');';
        addGlobalMethod(ValuesMethodHeader, @Lape_ValuesMethodHeader, Self);

        ValuesMethod := TLapeTree_Invoke.Create('Vals_' + FuncName, Self);
        ValuesMethod.addParam(TLapeTree_String.Create(FuncName, ValuesMethod));
        AllExprAdded := True;
        for i := 0 to Length(MethodDescr.AllObjList) - 1 do
        begin
          Decl := getDeclaration(MethodDescr.AllObjList[i].StackVar.Name,Result.StackInfo, true);

          ExprBase := nil;
          DocPos := getDocPos;
          if (Decl <> nil) then
            if (Decl is TLapeGlobalVar) then
              ExprBase := TLapeTree_GlobalVar.Create(TLapeGlobalVar(Decl), Self, @DocPos)
            else if (Decl is TLapeVar) then
              ExprBase := TlapeTree_ResVar.Create(_ResVar.New(TLapeVar(Decl)), Self, @DocPos)
            else if (Decl is TLapeType) then
              ExprBase := TLapeTree_VarType.Create(TLapeType(Decl), Self, @DocPos);

          if not Assigned(ExprBase) then
          begin
            AllExprAdded := False;
            Break;
          end;
          ValuesMethod.addParam(ExprBase);
        end;
        if AllExprAdded then
          Result.Statements.addStatement(ValuesMethod, True);
      end;


      EnterMethod := TLapeTree_Invoke.Create('_EnterMethod', Self);
      EnterMethod.addParam(TLapeTree_String.Create(FuncHeader.Name, EnterMethod));

      Test := TLapeTree_Operator.Create(op_cmp_NotEqual, Self);
      Test.Left := TLapeTree_InternalMethod_GetExceptionMessage.Create(Test);
      Test.Right := TLapeTree_String.Create('', Test);

      LeaveMethod := TLapeTree_Invoke.Create('_LeaveMethod', Self);
      LeaveMethod.addParam(TLapeTree_String.Create(FuncHeader.Name, LeaveMethod));
      LeaveMethod.addParam(Test);

      Result.Statements.addStatement(EnterMethod, True);

      Statement := TLapeTree_Try.Create(Self);
      Statement.Body := Result.Statements;
      Statement.FinallyBody := LeaveMethod;

      Result.Statements := TLapeTree_StatementList.Create(Self);
      Result.Statements.AddStatement(Statement);
    end;
  end;
end;

{$EndRegion}

{$Region 'TLapeCompilerDebug GetVars'}
function TLapeCompilerDebug.CallStack : TStringList;
var CallStackItem : String;
begin
  fCallStack.Clear;
    if CallStackNamesList.Count > 0 then
      for CallStackItem in CallStackNamesList do
        fCallStack.Insert(0,CallStackItem+'()');
    fCallStack.Add('!MAIN()');
    Result := fCallStack;
end;

function TLapeCompilerDebug.CurrentMethodParams : TArray<TVarOffset>;
var MethodDescr : TMethodDescr;
    MethodName : String;
begin
  if CallStackNamesList.Count = 0 then
    Exit;
  MethodName := CallStackNamesList.Last;

  if not MethodsList.ContainsKey(MethodName) then
    Exit;
  MethodDescr := MethodsList.Items[MethodName];

  Result := Result + MethodDescr.ParamsList;
end;

function TLapeCompilerDebug.CurrentMethodVars : TArray<TVarOffset>;
var MethodDescr : TMethodDescr;
    MethodName : String;
begin
  if CallStackNamesList.Count = 0 then
    Exit;
  MethodName := CallStackNamesList.Last;

  if not MethodsList.ContainsKey(MethodName) then
    Exit;
  MethodDescr := MethodsList.Items[MethodName];

  Result := MethodDescr.VarsList;
end;

function TLapeCompilerDebug.CurrentMethodHasResult : Boolean;
var MethodDescr : TMethodDescr;
    MethodName : String;
begin
  Result := False;
  if CallStackNamesList.Count = 0 then
    Exit;

  MethodName := CallStackNamesList.Last;

  if not MethodsList.ContainsKey(MethodName) then
    Exit;
  MethodDescr := MethodsList.Items[MethodName];
  Result := MethodDescr.isFunction;
end;

function TLapeCompilerDebug.CurrentMethodResult : TVarOffset;
var MethodDescr : TMethodDescr;
    MethodName : String;
begin
  if CallStackNamesList.Count = 0 then
    Exit;
  MethodName := CallStackNamesList.Last;

  if not MethodsList.ContainsKey(MethodName) then
    Exit;
  MethodDescr := MethodsList.Items[MethodName];

  Result := MethodDescr.Result;
end;

function TLapeCompilerDebug.GlobalVars : TArray<TLapeGlobalVar>;
var LapeDeclaration : TLapeDeclaration;
    LapeDeclArray : TLapeDeclArray;
begin
//as i see, this array is constant...but addresses of values can be changed!
//So, need to reform list all the time? or not? Olly will give an answer.
  LapeDeclArray := GlobalDeclarations.ExportToArray;
  if Length(LapeDeclArray) = 0 then
    Exit;
  for LapeDeclaration in LapeDeclArray do
  begin
    if (LapeDeclaration is TLapeGlobalVar)
      and (not TLapeGlobalVar(LapeDeclaration).isConstant)
      and (TLapeGlobalVar(LapeDeclaration).VarType.BaseType <> ltImportedMethod)
      and (not LapeDeclaration.DocPos.FileName.StartsWith('!'))
      and (not LapeDeclaration.Name.StartsWith('_'))
      //something raised from deep old times...
//      and (not ((LapeDeclaration.Name = 'RandSeed') or (LapeDeclaration.Name = 'ToString')))
      and (LapeDeclaration.DocPos.FileName = fScriptFileName)
      then
        Result := Result + [TLapeGlobalVar(LapeDeclaration)];
  end;
//+ ManagedType = records???
end;


{$EndRegion}

{$Region 'TLapeCompilerDebug common'}

constructor TLapeCompilerDebug.Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    );
begin
  MethodsList := TDictionary<String,TMethodDescr>.Create;
  CallStackNamesList := TList<String>.Create;
  fScriptFileName := ATokenizer.FileName;
  fCallStack := TStringList.Create;
  inherited;
//  FInternalMethodMap['SetEventProc'] := TLapeTree_InternalMethod_SetEventProc;
end;

destructor TLapeCompilerDebug.Destroy;
begin
  if Assigned(MethodsList) then
    MethodsList.Free();
  if Assigned(CallStackNamesList) then
    CallStackNamesList.Free;
  fCallStack.Free;
  inherited;
end;
{$EndRegion}

end.
