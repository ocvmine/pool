UNIT coreunit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, dateutils, IdTCPClient, IdGlobal, MD5, StrUtils, mpsignerutils,
  base64, HlpHashFactory, nosodig.crypto, process;

TYPE

  TTGetNodeStatus = class(TThread)
   private
     Slot: Integer;
   protected
     procedure Execute; override;
   public
     constructor Create(const CreatePaused: Boolean;TSlot:Integer);
   end;

  TConsensusData = packed record
    Value : string[40];
    count : integer;
    end;

  DivResult = packed record
     cociente : string[255];
     residuo : string[255];
     end;

  TNodeData = packed record
    host : string[60];
    port : integer;
    block : integer;
    Pending: integer;
    Branch : String[40];
    MNsHash : string[5];
    MNsCount : integer;
    Updated : integer;
    LBHash : String[32];
    NMSDiff : String[32];
    LBTimeEnd : Int64;
    LBMiner   : String;
    LBPoW     : int64;
    LBSolDiff : string[32];
    end;

Procedure DoNothing();
Function Parameter(LineText:String;ParamNumber:int64):String;
Function UTCTime():int64;
function Int2Curr(Value: int64): string;
Function BestHashReadeable(BestDiff:String):string;
Function GetDiffHashrate(bestdiff:String):integer;
Function GetConsensus():Boolean;
Procedure LoadNodes(linextext:string);
Procedure SetMainConsensus(New:TnodeData);
Function GetMainConsensus():TNodeData;
Function GetNodeIndex(Index:integer):TNodeData;
Procedure CloseSyncingThread();
Function SyncingThreadsValue():Integer;
Function NosoHashOld(source:string):string;
Function HashMD5String(StringToHash:String):String;
Function CheckHashDiffOld(Target,ThisHash:String):string;
//*** CRYPTO FUNCTIONS ***
Function KeysMatch(PublicK,PrivateK:String):boolean;
function GetStringSigned(StringtoSign, PrivateKey:String):String;
function VerifySignedString(StringToVerify,B64String,PublicKey:String):boolean;
function IsValidHashAddress(Address:String):boolean;
function IsValid58(base58text:string):boolean;
function GetAddressFromPublicKey(PubKey:String):String;
function HashSha256String(StringToHash:string):string;
function HashMD160String(StringToHash:string):String;
function GetTransferHash(TextLine:string):String;
function GetOrderHash(TextLine:string):String;
function GetFee(monto:int64):Int64;
//*** BIG MATHS ***
function BMDecTo58(numero:string):string;
function BMB58resumen(numero58:string):string;
Function BMDividir(Numero1,Numero2:string):DivResult;
function ClearLeadingCeros(numero:string):string;
function BMHexTo58(numerohex:string;alphabetnumber:integer):string;
function BMHexToDec(numerohex:string):string;
Function BMExponente(Numero1,Numero2:string):string;
Function BMMultiplicar(Numero1,Numero2:string):string;
function BMAdicion(numero1,numero2:string):string;
Function PonerCeros(numero:String;cuantos:integer):string;
//***************
Function HashrateToShow(speed:int64):String;
function GetMainnetTimestamp(Trys:integer=5):int64;
Procedure RunExternalProgram(ProgramToRun:String);
Function GetVerificators(LineText:String):String;
function GetMNsFromNode(Trys:integer=5):string;

CONST
  HasheableChars = '!"#$%&#39)*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  B58Alphabet : string = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
  B36Alphabet : string = '0123456789abcdefghijklmnopqrstuvwxyz';
  HexAlphabet : string = '0123456789ABCDEF';

var
  OffSet        : int64 = 0;
  MainConsensus : TNodeData;
  ConsensusTime : int64;
  SyncingThreads: Integer;
  ContactedNodes: Integer;
  LengthNodes   : Integer;
  CS_NodesArray : TRTLCriticalSection;
  CS_CSThread   : TRTLCriticalSection;
  CS_Consensus  : TRTLCriticalSection;
  NodesArray    : Array of TNodeData;
  DefaultNodes  : String =  'DefNodes '+
                            '109.230.238.240 '+
                            '149.57.235.14 '+
                            '149.57.226.244 '+
                            '81.22.38.101 '+
                            '66.151.117.247 '+
                            '149.57.229.81 '+
                            '149.57.242.211 '+
                            '149.57.138.12 '+
                            '159.196.1.198 '+
                            '101.100.138.125';

IMPLEMENTATION

constructor TTGetNodeStatus.Create(const CreatePaused: Boolean; TSlot:Integer);
begin
inherited Create(CreatePaused);
Slot := TSlot;
FreeOnTerminate := True;
end;

procedure TTGetNodeStatus.Execute;
var
  TCPClient : TidTCPClient;
  LineText  : String = '';
  Sucess    : Boolean = false;
  ThisNode  : TNodeData;
Begin
ThisNode := GetNodeIndex(Slot);
TCPClient := TidTCPClient.Create(nil);
TCPclient.Host:=ThisNode.host;
TCPclient.Port:=ThisNode.port;
TCPclient.ConnectTimeout:= 500;
TCPclient.ReadTimeout:=500;
TRY
TCPclient.Connect;
TCPclient.IOHandler.WriteLn('NODESTATUS');
LineText := TCPclient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
TCPclient.Disconnect();
ContactedNodes := ContactedNodes+1;
Sucess := true;
EXCEPT on E:Exception do
   Sucess := false;
END{try};
TCPClient.Free;
if sucess then
   begin
   ThisNode.block    :=StrToIntDef(Parameter(LineText,2),ThisNode.block);
   ThisNode.MNsHash  :=Parameter(LineText,8);
   ThisNode.LBHash   :=Parameter(LineText,10);
   ThisNode.NMSDiff  :=Parameter(LineText,11);
   ThisNode.LBTimeEnd:=StrToInt64Def(Parameter(LineText,12),0);
   ThisNode.LBMiner  :=Parameter(LineText,13);
   ThisNode.LBPoW    :=StrToInt64Def(Parameter(LineText,15),0);
   ThisNode.LBSolDiff:=Parameter(LineText,16);
   EnterCriticalSection(CS_NodesArray);
   NodesArray[slot] := ThisNode;
   LeaveCriticalSection(CS_NodesArray);
   end;
CloseSyncingThread;
//write('*');
End;

Procedure DoNothing();
Begin
// Implemented for debug
End;

Function Parameter(LineText:String;ParamNumber:int64):String;
var
  Temp : String = '';
  ThisChar : Char;
  Contador : int64 = 1;
  WhiteSpaces : int64 = 0;
  parentesis : boolean = false;
Begin
while contador <= Length(LineText) do
   begin
   ThisChar := Linetext[contador];
   if ((thischar = '(') and (not parentesis)) then parentesis := true
   else if ((thischar = '(') and (parentesis)) then
      begin
      result := '';
      exit;
      end
   else if ((ThisChar = ')') and (parentesis)) then
      begin
      if WhiteSpaces = ParamNumber then
         begin
         result := temp;
         exit;
         end
      else
         begin
         parentesis := false;
         temp := '';
         end;
      end
   else if ((ThisChar = ' ') and (not parentesis)) then
      begin
      WhiteSpaces := WhiteSpaces +1;
      if WhiteSpaces > Paramnumber then
         begin
         result := temp;
         exit;
         end;
      end
   else if ((ThisChar = ' ') and (parentesis) and (WhiteSpaces = ParamNumber)) then
      begin
      temp := temp+ ThisChar;
      end
   else if WhiteSpaces = ParamNumber then temp := temp+ ThisChar;
   contador := contador+1;
   end;
if temp = ' ' then temp := '';
Result := Temp;
End;

// Returns the UTCTime
Function UTCTime():int64;
var
  G_TIMELocalTimeOffset : int64;
  GetLocalTimestamp : int64;
  UnixTime : int64;
Begin
result := 0;
G_TIMELocalTimeOffset := GetLocalTimeOffset*60;
GetLocalTimestamp := DateTimeToUnix(now);
UnixTime := GetLocalTimestamp+G_TIMELocalTimeOffset;
result := UnixTime-Offset;
End;

function Int2Curr(Value: int64): string;
begin
Result := IntTostr(Abs(Value));
result :=  AddChar('0',Result, 9);
Insert('.',Result, Length(Result)-7);
If Value <0 THen Result := '-'+Result;
end;

Function BestHashReadeable(BestDiff:String):string;
var
  counter :integer = 0;
Begin
if bestdiff = '' then BestDiff := 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
repeat
  counter := counter+1;
until bestdiff[counter]<> '0';
Result := (Counter-1).ToString+'.';
if counter<length(BestDiff) then Result := Result+bestdiff[counter];
End;

Function GetDiffHashrate(bestdiff:String):integer;
var
  counter :integer = 0;
Begin
result := 0;
repeat
  counter := counter+1;
until bestdiff[counter]<> '0';
Result := (Counter-1)*100;
if bestdiff[counter]='1' then Result := Result+87;
if bestdiff[counter]='2' then Result := Result+75;
if bestdiff[counter]='3' then Result := Result+50;
if bestdiff[counter]='4' then Result := Result+37;
if bestdiff[counter]='5' then Result := Result+25;
End;

Function GetConsensus():Boolean;
var
  Counter   : integer;
  UseThread : TTGetNodeStatus;
  ArrT      : array of TConsensusData;
  ResNode   : TNodeData;
  TimeStart : int64;

  function GetHighest():string;
   var
     maximum : integer = 0;
     counter : integer;
     MaxIndex : integer = 0;
   Begin
   for counter := 0 to length(ArrT)-1 do
      begin
      if ArrT[counter].count> maximum then
         begin
         maximum := ArrT[counter].count;
         MaxIndex := counter;
         end;
      end;
   result := ArrT[MaxIndex].Value;
   End;

   Procedure AddValue(Tvalue:String);
   var
     counter : integer;
     added : Boolean = false;
     ThisItem : TConsensusData;
   Begin
   for counter := 0 to length(ArrT)-1 do
      begin
      if Tvalue = ArrT[counter].Value then
         begin
         ArrT[counter].count+=1;
         Added := true;
         end;
      end;
   if not added then
      begin
      ThisItem.Value:=Tvalue;
      ThisItem.count:=1;
      Insert(ThisITem,ArrT,length(ArrT));
      end;
   End;

Begin
Result := False;
ContactedNodes := 0;
SyncingThreads := LengthNodes;
TimeStart := GetTickCount64;
For Counter := 0 to LengthNodes-1 do
   begin
   UseThread := TTGetNodeStatus.Create(True,counter);
   UseThread.FreeOnTerminate:=true;
   UseThread.Start;
   Sleep(1);
   end;
REPEAT
   sleep(1);
UNTIL SyncingThreadsValue <= 0;
ConsensusTime := GetTickCount64-TimeStart;
If ContactedNodes>=(LengthNodes div 2) then
   begin
   ResNode := Default(TNodeData);
   MainConsensus := Default(TNodeData);
   // Get the consensusblock
   For counter := 0 to LengthNodes-1 do
      begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).block.ToString);
         ResNode.block := GetHighest.ToInteger;
         end;
      end;
   // Get the consensus LBHash
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).LBHash);
         ResNode.LBHash := GetHighest;
         end;
      End;
   // Get the consensus last block time end
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).LBTimeEnd.ToString);
         ResNode.LBTimeEnd := GetHighest.ToInt64;
         end;
      End;
   // Get the consensus last block Miner
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).LBMiner);
         ResNode.LBMiner := GetHighest;
         end;
      End;
   // Get the consensus last block PoW
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).LBPoW.ToString);
         ResNode.LBPoW := GetHighest.ToInt64;
         end;
      End;
   // Get the consensus last block SolDiff
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).LBSolDiff);
         ResNode.LBSolDiff := GetHighest;
         end;
      End;

   // Get the consensus MNs Hash
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      if GetNodeIndex(counter).block>0 then
         begin
         AddValue(GetNodeIndex(counter).MNsHash);
         ResNode.MNsHash := GetHighest;
         end;
      End;

   {More fields to get update}

   if ResNode.block>GetMainConsensus.block then
      begin
      result := true;
      SetMainConsensus(ResNode);
      end;
   end;
End;

Procedure LoadNodes(linextext:string);
var
  ThisNode : String  = '';
  Counter  : integer = 0;
  NewNode : TNodeData;
Begin
if linextext <> DefaultNodes then
   linextext := GetVerificators(linextext);
EnterCriticalSection(CS_NodesArray);
SetLength(NodesArray,0);
REPEAT
   Begin
   ThisNode := '';
   ThisNode := Parameter(linextext,counter);
   If ThisNode <> '' then
      begin
      NewNode := Default(TNodeData);
      ThisNode := StringReplace(ThisNode,':',' ',[rfReplaceAll, rfIgnoreCase]);
      ThisNode := StringReplace(ThisNode,';',' ',[rfReplaceAll, rfIgnoreCase]);
      NewNode.host:=Parameter(ThisNode,0);
      NewNode.port:=StrToIntDef(Parameter(ThisNode,1),8080);
      Insert(NewNode,NodesArray,length(NodesArray));
      end;
   Counter := Counter+1;
   End;
UNTIL ThisNode = '';
LengthNodes := length(NodesArray);
LeaveCriticalSection(CS_NodesArray);
End;

Procedure SetMainConsensus(New:TnodeData);
Begin
EnterCriticalSection(CS_Consensus);
MainConsensus := New;
LeaveCriticalSection(CS_Consensus);
End;

Function GetMainConsensus():TNodeData;
Begin
EnterCriticalSection(CS_Consensus);
Result := MainConsensus;
LeaveCriticalSection(CS_Consensus);
End;

Function GetNodeIndex(Index:integer):TNodeData;
Begin
EnterCriticalSection(CS_NodesArray);
Result := NodesArray[index];
LeaveCriticalSection(CS_NodesArray);
End;

Procedure CloseSyncingThread();
Begin
EnterCriticalSection(CS_CSThread);
Dec(SyncingThreads);
LeaveCriticalSection(CS_CSThread);
End;

Function SyncingThreadsValue():Integer;
Begin
EnterCriticalSection(CS_CSThread);
Result := SyncingThreads;
LeaveCriticalSection(CS_CSThread);
End;

Function NosoHashOld(source:string):string;
var
  counter : integer;
  FirstChange : array[1..128] of string;
  finalHASH : string;
  ThisSum : integer;
  charA,charB,charC,charD:integer;
  Filler : string = '%)+/5;=CGIOSYaegk';

  Function GetClean(number:integer):integer;
  Begin
  result := number;
  if result > 126 then
     begin
     repeat
       result := result-95;
     until result <= 126;
     end;
  End;

  function RebuildHash(incoming : string):string;
  var
    counter : integer;
    resultado2 : string = '';
    chara,charb, charf : integer;
  Begin
  for counter := 1 to length(incoming) do
     begin
     chara := Ord(incoming[counter]);
       if counter < Length(incoming) then charb := Ord(incoming[counter+1])
       else charb := Ord(incoming[1]);
     charf := chara+charb; CharF := GetClean(CharF);
     resultado2 := resultado2+chr(charf);
     end;
  result := resultado2
  End;

Begin
result := '';
for counter := 1 to length(source) do
   if ((Ord(source[counter])>126) or (Ord(source[counter])<33)) then
      begin
      source := '';
      break
      end;
if length(source)>63 then source := '';
repeat source := source+filler;
until length(source) >= 128;
source := copy(source,0,128);
FirstChange[1] := RebuildHash(source);
for counter := 2 to 128 do FirstChange[counter]:= RebuildHash(firstchange[counter-1]);
finalHASH := FirstChange[128];
for counter := 0 to 31 do
   begin
   charA := Ord(finalHASH[(counter*4)+1]);
   charB := Ord(finalHASH[(counter*4)+2]);
   charC := Ord(finalHASH[(counter*4)+3]);
   charD := Ord(finalHASH[(counter*4)+4]);
   thisSum := CharA+charB+charC+charD;
   ThisSum := GetClean(ThisSum);
   Thissum := ThisSum mod 16;
   result := result+IntToHex(ThisSum,1);
   end;
Result := HashMD5String(Result);
End;

Function HashMD5String(StringToHash:String):String;
Begin
result := Uppercase(MD5Print(MD5String(StringToHash)));
end;

Function CheckHashDiffOld(Target,ThisHash:String):string;
var
   counter : integer;
   ValA, ValB, Diference : Integer;
   ResChar : String;
   Resultado : String = '';
Begin
result := 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
for counter := 1 to 32 do
   begin
   ValA := Hex2Dec(ThisHash[counter]);
   ValB := Hex2Dec(Target[counter]);
   Diference := Abs(ValA - ValB);
   ResChar := UPPERCASE(IntToHex(Diference,1));
   Resultado := Resultado+ResChar
   end;
Result := Resultado;
End;

//************************
//*** CRYPTO FUNCTIONS ***
//************************

// Verify is a keys pair match
Function KeysMatch(PublicK,PrivateK:String):boolean;
Begin
Result := VerifySignedString('VERIFICATION',GetStringSigned('VERIFICATION',PrivateK),PublicK);
End;

// Signs a message with the given private key
function GetStringSigned(StringtoSign, PrivateKey:String):String;
var
  Signature, MessageAsBytes: TBytes;
Begin
Result := '';
TRY
MessageAsBytes :=StrToByte(DecodeStringBase64(StringtoSign));
Signature := TSignerUtils.SignMessage(MessageAsBytes, StrToByte(DecodeStringBase64(PrivateKey)),
      TKeyType.SECP256K1);
Result := EncodeStringBase64(ByteToString(Signature));
EXCEPT ON E:Exception do
   begin
   result := '';
   end;
END{Try};
End;

// Verify if a signed string is valid
function VerifySignedString(StringToVerify,B64String,PublicKey:String):boolean;
var
  Signature, MessageAsBytes: TBytes;
Begin
result := false;
TRY
MessageAsBytes := StrToByte(DecodeStringBase64(StringToVerify));
Signature := StrToByte(DecodeStringBase64(B64String));
Result := TSignerUtils.VerifySignature(Signature, MessageAsBytes,
      StrToByte(DecodeStringBase64(PublicKey)), TKeyType.SECP256K1);
EXCEPT ON E:Exception do
   begin
   Result := false;
   end;
END{Try};
End;

// Checks if a string is a valid address hash
function IsValidHashAddress(Address:String):boolean;
var
  OrigHash : String;
  Clave:String;
Begin
result := false;
if ((length(address)>20) and (address[1] = 'N')) then
   begin
   OrigHash := Copy(Address,2,length(address)-3);
   if IsValid58(OrigHash) then
      begin
      Clave := BMDecTo58(BMB58resumen(OrigHash));
      OrigHash := 'N'+OrigHash+clave;
      if OrigHash = Address then result := true else result := false;
      end;
   end
End;

function IsValid58(base58text:string):boolean;
var
  counter : integer;
Begin
result := true;
if length(base58text) > 0 then
   begin
   for counter := 1 to length(base58text) do
      begin
      if pos (base58text[counter],B58Alphabet) = 0 then
         begin
         result := false;
         break;
         end;
      end;
   end
else result := false;
End;

// RETURNS AN ADDRESS FROM A PUBLIC LEY
function GetAddressFromPublicKey(PubKey:String):String;
var
  PubSHAHashed,Hash1,Hash2,clave:String;
  sumatoria : string;
Begin
PubSHAHashed := HashSha256String(PubKey);
Hash1 := HashMD160String(PubSHAHashed);
hash1 := BMHexTo58(Hash1,58);
sumatoria := BMB58resumen(Hash1);
clave := BMDecTo58(sumatoria);
hash2 := hash1+clave;
Result := 'N'+hash2;
End;

// RETURNS THE SHA256 OF A STRING
function HashSha256String(StringToHash:string):string;
begin
result :=
THashFactory.TCrypto.CreateSHA2_256().ComputeString(StringToHash, TEncoding.UTF8).ToString();
end;

// RETURNS HASH MD160 OF A STRING
function HashMD160String(StringToHash:string):String;
Begin
result :=
THashFactory.TCrypto.CreateRIPEMD160().ComputeString(StringToHash, TEncoding.UTF8).ToString();
End;

// Returns a transfer hash
function GetTransferHash(TextLine:string):String;
var
  Resultado : String = '';
  Sumatoria, clave : string;
Begin
Resultado := HashSHA256String(TextLine);
Resultado := BMHexTo58(Resultado,58);
sumatoria := BMB58resumen(Resultado);
clave := BMDecTo58(sumatoria);
Result := 'tR'+Resultado+clave;
End;

// Returns a order hash
function GetOrderHash(TextLine:string):String;
Begin
Result := HashSHA256String(TextLine);
Result := 'OR'+BMHexTo58(Result,36);
End;

// Returns the fee
function GetFee(monto:int64):Int64;
Begin
Result := ((monto*10000) div 10001) div 10000;
if result < 10 then result := 10;
End;

//*****************
//*** BIG MATHS ***
//*****************

// CONVERTS A DECIMAL VALUE TO A BASE58 STRING
function BMDecTo58(numero:string):string;
var
  decimalvalue : string;
  restante : integer;
  ResultadoDiv : DivResult;
  Resultado : string = '';
Begin
decimalvalue := numero;
while length(decimalvalue) >= 2 do
   begin
   ResultadoDiv := BMDividir(decimalvalue,'58');
   DecimalValue := Resultadodiv.cociente;
   restante := StrToInt(ResultadoDiv.residuo);
   resultado := B58Alphabet[restante+1]+resultado;
   end;
if StrToInt(decimalValue) >= 58 then
   begin
   ResultadoDiv := BMDividir(decimalvalue,'58');
   DecimalValue := Resultadodiv.cociente;
   restante := StrToInt(ResultadoDiv.residuo);
   resultado := B58Alphabet[restante+1]+resultado;
   end;
if StrToInt(decimalvalue) > 0 then resultado := B58Alphabet[StrToInt(decimalvalue)+1]+resultado;
result := resultado;
End;

// RETURN THE SUMATORY OF A BASE58
function BMB58resumen(numero58:string):string;
var
  counter, total : integer;
Begin
total := 0;
for counter := 1 to length(numero58) do
   begin
   total := total+Pos(numero58[counter],B58Alphabet)-1;
   end;
result := IntToStr(total);
End;

// DIVIDES TO NUMBERS
Function BMDividir(Numero1,Numero2:string):DivResult;
var
  counter : integer;
  cociente : string = '';
  long : integer;
  Divisor : Int64;
  ThisStep : String = '';
Begin
long := length(numero1);
Divisor := StrToInt64(numero2);
for counter := 1 to long do
   begin
   ThisStep := ThisStep + Numero1[counter];
   if StrToInt(ThisStep) >= Divisor then
      begin
      cociente := cociente+IntToStr(StrToInt(ThisStep) div Divisor);
      ThisStep := (IntToStr(StrToInt(ThisStep) mod Divisor));
      end
   else cociente := cociente+'0';
   end;
result.cociente := ClearLeadingCeros(cociente);
result.residuo := ClearLeadingCeros(thisstep);
End;

// REMOVES LEFT CEROS
function ClearLeadingCeros(numero:string):string;
var
  count : integer = 0;
  movepos : integer = 0;
Begin
result := '';
if numero[1] = '-' then movepos := 1;
for count := 1+movepos to length(numero) do
   begin
   if numero[count] <> '0' then result := result + numero[count];
   if ((numero[count]='0') and (length(result)>0)) then result := result + numero[count];
   end;
if result = '' then result := '0';
if ((movepos=1) and (result <>'0')) then result := '-'+result;
End;

// Hex a base 58
function BMHexTo58(numerohex:string;alphabetnumber:integer):string;
var
  decimalvalue : string;
  restante : integer;
  ResultadoDiv : DivResult;
  Resultado : string = '';
  AlpahbetUsed : String;
Begin
AlpahbetUsed := B58Alphabet;
if alphabetnumber=36 then AlpahbetUsed := B36Alphabet;
decimalvalue := BMHexToDec(numerohex);
while length(decimalvalue) >= 2 do
   begin
   ResultadoDiv := BMDividir(decimalvalue,IntToStr(alphabetnumber));
   DecimalValue := Resultadodiv.cociente;
   restante := StrToInt(ResultadoDiv.residuo);
   resultado := AlpahbetUsed[restante+1]+resultado;
   end;
if StrToInt(decimalValue) >= alphabetnumber then
   begin
   ResultadoDiv := BMDividir(decimalvalue,IntToStr(alphabetnumber));
   DecimalValue := Resultadodiv.cociente;
   restante := StrToInt(ResultadoDiv.residuo);
   resultado := AlpahbetUsed[restante+1]+resultado;
   end;
if StrToInt(decimalvalue) > 0 then resultado := AlpahbetUsed[StrToInt(decimalvalue)+1]+resultado;
result := resultado;
End;

// HEX TO DECIMAL
function BMHexToDec(numerohex:string):string;
var
  DecValues : array of integer;
  ExpValues : array of string;
  MultipliValues : array of string;
  counter : integer;
  Long : integer;
  Resultado : string = '0';
Begin
Long := length(numerohex);
numerohex := uppercase(numerohex);
setlength(DecValues,0);
setlength(ExpValues,0);
setlength(MultipliValues,0);
setlength(DecValues,Long);
setlength(ExpValues,Long);
setlength(MultipliValues,Long);
for counter := 1 to Long do
   DecValues[counter-1] := Pos(NumeroHex[counter],HexAlphabet)-1;
for counter := 1 to long do
   ExpValues[counter-1] := BMExponente('16',IntToStr(long-counter));
for counter := 1 to Long do
   MultipliValues[counter-1] := BMMultiplicar(ExpValues[counter-1],IntToStr(DecValues[counter-1]));
for counter := 1 to long do
   Resultado := BMAdicion(resultado,MultipliValues[counter-1]);
result := resultado;
End;

// CALCULATES A EXPONENTIAL NUMBER
Function BMExponente(Numero1,Numero2:string):string;
var
  count : integer = 0;
  resultado : string = '';
Begin
if numero2 = '1' then result := numero1
else if numero2 = '0' then result := '1'
else
   begin
   resultado := numero1;
   for count := 2 to StrToInt(numero2) do
      resultado := BMMultiplicar(resultado,numero1);
   result := resultado;
   end;
End;

// MULTIPLIER
Function BMMultiplicar(Numero1,Numero2:string):string;
var
  count,count2 : integer;
  sumandos : array of string;
  thiscol : integer;
  carry: integer = 0;
  cantidaddeceros : integer = 0;
  TotalSuma : string = '0';
Begin
setlength(sumandos,length(numero2));
for count := length(numero2) downto 1 do
   begin
   for count2 := length(numero1) downto 1 do
      begin
      thiscol := (StrToInt(numero2[count]) * StrToInt(numero1[count2])+carry);
      carry := thiscol div 10;
      ThisCol := ThisCol - (carry*10);
      sumandos[cantidaddeceros] := IntToStr(thiscol)+ sumandos[cantidaddeceros];
      end;
   if carry > 0 then sumandos[cantidaddeceros] := IntToStr(carry)+sumandos[cantidaddeceros];
   carry := 0;
   sumandos[cantidaddeceros] := PonerCeros(sumandos[cantidaddeceros],cantidaddeceros);
   cantidaddeceros := cantidaddeceros+1;
   end;
for count := 0 to length(sumandos)-1 do
   TotalSuma := BMAdicion(Sumandos[count],totalsuma);
result := ClearLeadingCeros(TotalSuma);
End;

// ADDS 2 NUMBERS
function BMAdicion(numero1,numero2:string):string;
var
  longitude : integer = 0;
  count: integer = 0;
  carry : integer = 0;
  resultado : string = '';
  thiscol : integer;
  ceros : integer;
Begin
longitude := length(numero1);
if length(numero2)>longitude then
   begin
   longitude := length(numero2);
   ceros := length(numero2)-length(numero1);
   while count < ceros do
      begin
      numero1 := '0'+numero1;
      count := count+1;
      end;
   end
else
   begin
   ceros := length(numero1)-length(numero2);
      while count < ceros do
      begin
      numero2 := '0'+numero2;
      count := count+1;
      end;
   end;
for count := longitude downto 1 do
   Begin
   thiscol := StrToInt(numero1[count]) + StrToInt(numero2[count])+carry;
   carry := 0;
   if thiscol > 9 then
      begin
      thiscol := thiscol-10;
      carry := 1;
      end;
   resultado := inttoStr(thiscol)+resultado;
   end;
if carry > 0 then resultado := '1'+resultado;
result := resultado;
End;

// DRAW CEROS FOR MULTIPLICATION
Function PonerCeros(numero:String;cuantos:integer):string;
var
  contador : integer = 0;
  NewNumber : string;
Begin
NewNumber := numero;
while contador < cuantos do
   begin
   NewNumber := NewNumber+'0';
   contador := contador+1;
   end;
result := NewNumber;
End;

Function HashrateToShow(speed:int64):String;
Begin
if speed>1000000000 then result := FormatFloat('0.00',speed/1000000000)+' Gh/s'
else if speed>1000000 then result := FormatFloat('0.00',speed/1000000)+' Mh/s'
else if speed>1000 then result := FormatFloat('0.00',speed/1000)+' Kh/s'
else result := speed.ToString+' h/s'
End;

function GetMainnetTimestamp(Trys:integer=5):int64;
var
  Client : TidTCPClient;
  RanNode : integer;
  ThisNode : TNodeData;
  WasDone : boolean = false;
Begin
Result := 0;
REPEAT
   RanNode := Random(LengthNodes);
   ThisNode := GetNodeIndex(RanNode);
   Client := TidTCPClient.Create(nil);
   Client.Host:=ThisNode.host;
   Client.Port:=ThisNode.port;
   Client.ConnectTimeout:= 3000;
   Client.ReadTimeout:= 3000;
   TRY
   Client.Connect;
   Client.IOHandler.WriteLn('NSLTIME');
   Result := StrToInt64Def(Client.IOHandler.ReadLn(IndyTextEncoding_UTF8),0);
   WasDone := true;
   EXCEPT on E:Exception do
      begin
      WasDone := False;
      end;
   END{Try};
Inc(Trys);
UNTIL ( (WasDone) or (Trys = 5) );
if client.Connected then Client.Disconnect();
client.Free;
End;

Procedure RunExternalProgram(ProgramToRun:String);
var
  Process: TProcess;
  I: Integer;
begin
Process := TProcess.Create(nil);
   TRY
   Process.InheritHandles := False;
   Process.Options := [];
   Process.ShowWindow := swoShow;
   for I := 1 to GetEnvironmentVariableCount do
      Process.Environment.Add(GetEnvironmentString(I));
   {$IFDEF UNIX}
   process.Executable := 'bash';
   process.Parameters.Add(ProgramToRun);
   {$ENDIF}
   {$IFDEF WINDOWS}
   Process.Executable := ProgramToRun;
   {$ENDIF}
   Process.Execute;
   EXCEPT ON E:Exception do

   END; {TRY}
Process.Free;
End;

Function GetVerificators(LineText:String):String;
type
  TMNData     = packed record
   ip      : string[15];
   port    : integer;
   Address : string[32];
   Count   : integer;
   end;
var
  counter   : integer = 1;
  count2    : integer;
  ThisParam : string;
  ThisMN    : TMnData;
  Ip        : string;
  Port      : integer;
  Address   : string;
  Count     : integer;
  ArrNodes  : array of TMnData;
  Added     : boolean;
  VersCount : integer;
Begin
result := '';
SetLEngth(ArrNodes,0);
repeat
  thisParam := Parameter(LineText,counter);
  if ThisParam <>'' then
     begin
     Added := false;
     ThisParam := StringReplace(ThisParam,':',' ',[rfReplaceAll, rfIgnoreCase]);
     ThisParam := StringReplace(ThisParam,';',' ',[rfReplaceAll, rfIgnoreCase]);
     ThisMN.Ip := Parameter(ThisParam,0);
     ThisMN.Port := StrToIntDef(Parameter(ThisParam,1),8080);
     ThisMN.Address := Parameter(ThisParam,2);
     ThisMN.Count   := StrToIntDef(Parameter(ThisParam,3),1);
     if Length(ArrNodes) = 0 then Insert(ThisMN,ArrNodes,0)
     else
        begin
        for count2 := 0 to length(ArrNodes)-1 do
           begin
           if ThisMN.count > ArrNodes[count2].count then
              begin
              Insert(ThisMN,ArrNodes,count2);
              Added := true;
              Break;
              end;
           end;
        if not Added then Insert(ThisMN,ArrNodes,length(ArrNodes));
        end;
     end;
  Inc(Counter);
until thisParam = '';
VersCount := (length(ArrNodes) div 10)+3;
//if VersCount<Length(ArrNodes) then VersCount := Length(ArrNodes);
Delete(ArrNodes,VersCount,Length(ArrNodes));
for counter := 0 to length(ArrNodes)-1 do
   begin
   Result := Result+ ArrNodes[counter].ip+';'+IntToStr(ArrNodes[counter].port)+':'+ArrNodes[counter].address+':'+
             IntToStr(ArrNodes[counter].count)+' ';
   end;
Result := Trim(Result);
End;

function GetMNsFromNode(Trys:integer=5):string;
var
  Client : TidTCPClient;
  RanNode : integer;
  ThisNode : TNodeData;
  WasDone : boolean = false;
Begin
Result := '';
Client := TidTCPClient.Create(nil);
REPEAT
   RanNode := Random(LengthNodes);
   ThisNode := GetNodeIndex(RanNode);
   Client.Host:=ThisNode.host;
   Client.Port:=ThisNode.port;
   Client.ConnectTimeout:= 3000;
   Client.ReadTimeout:= 3000;
   TRY
   Client.Connect;
   Client.IOHandler.WriteLn('NSLMNS');
   Result := Client.IOHandler.ReadLn(IndyTextEncoding_UTF8);
   WasDone := true;
   EXCEPT on E:Exception do
      begin
      WasDone := False;
      end;
   END{Try};
Inc(Trys);
UNTIL ( (WasDone) or (Trys = 5) );
if client.Connected then Client.Disconnect();
client.Free;

End;

{
Procedure SaveNewNodes(TLine:String);
var
  ThisFile : TextFile;
Begin
result := '';
AssignFile(ThisFile,'nodes.txt');
TRY
reset(ThisFile);
Readln(ThisFile,result);
PreviousNodes := Result;
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION do
   begin
   result := previousNodes
   end;
END {TRY};
End;
}

INITIALIZATION
InitCriticalSection(CS_NodesArray);
InitCriticalSection(CS_CSThread);
InitCriticalSection(CS_Consensus);


FINALIZATION
DoneCriticalSection(CS_NodesArray);
DoneCriticalSection(CS_CSThread);
DoneCriticalSection(CS_Consensus);

END. // End unit

