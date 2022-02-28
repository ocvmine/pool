UNIT coreunit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, dateutils, IdTCPClient, IdGlobal, MD5, StrUtils;

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
    end;

Procedure DoNothing();
Function Parameter(LineText:String;ParamNumber:int64):String;
Function UTCTime():int64;
Function GetConsensus():Boolean;
Procedure LoadNodes();
Procedure SetMainConsensus(New:TnodeData);
Function GetMainConsensus():TNodeData;
Function GetNodeIndex(Index:integer):TNodeData;
Procedure CloseSyncingThread();
Function NosoHash(source:string):string;
Function HashMD5String(StringToHash:String):String;
Function CheckHashDiff(Target,ThisHash:String):string;

CONST
  HasheableChars = '!"#$%&'')*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';

var
  MainConsensus : TNodeData;
  SyncingThreads: Integer;
  ContactedNodes: Integer;
  LengthNodes   : Integer;
  CS_NodesArray : TRTLCriticalSection;
  CS_CSThread   : TRTLCriticalSection;
  CS_Consensus  : TRTLCriticalSection;
  NodesArray    : Array of TNodeData;
  DefaultNodes  : String = '23.94.21.83:8080 '+'45.146.252.103:8080 '+'107.172.5.8:8080 '+
                          '109.230.238.240:8080 '+'172.245.52.208:8080 '+'192.210.226.118:8080 '+'194.156.88.117:8080';

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
TCPclient.ConnectTimeout:= 3000;
TCPclient.ReadTimeout:=3000;
TRY
TCPclient.Connect;
TCPclient.IOHandler.WriteLn('NODESTATUS');
LineText := TCPclient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
TCPclient.Disconnect();
ContactedNodes := ContactedNodes+1;
Sucess := true;
EXCEPT on E:Exception do DoNothing
END{try};
TCPClient.Free;
if sucess then
   begin
   ThisNode.block:=StrToIntDef(Parameter(LineText,2),ThisNode.block);
   ThisNode.LBHash:=Parameter(LineText,10);
   ThisNode.NMSDiff:=Parameter(LineText,11);
   ThisNode.LBTimeEnd:=StrToInt64Def(Parameter(LineText,12),0);
   ThisNode.LBMiner:=Parameter(LineText,13);
   EnterCriticalSection(CS_NodesArray);
   NodesArray[slot] := ThisNode;
   LeaveCriticalSection(CS_NodesArray);
   end;
CloseSyncingThread;
write('*');
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
result := UnixTime;
End;

Function GetConsensus():Boolean;
var
  Counter  : integer;
  UseThread: TTGetNodeStatus;
  ArrT     : array of TConsensusData;
  ResNode  : TNodeData;

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
For Counter := 0 to LengthNodes-1 do
   begin
   UseThread := TTGetNodeStatus.Create(True,counter);
   UseThread.FreeOnTerminate:=true;
   UseThread.Start;
   Sleep(1);
   end;
REPEAT
   sleep(1);
UNTIL SyncingThreads <= 0;
If ContactedNodes>=(LengthNodes div 2)+1 then
   begin
   ResNode := Default(TNodeData);
   MainConsensus := Default(TNodeData);
   // Get the consensusblock
   For counter := 0 to LengthNodes-1 do
      begin
      AddValue(GetNodeIndex(counter).block.ToString);
      ResNode.block := GetHighest.ToInteger;
      end;
   // Get the consensus LBHash
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      AddValue(GetNodeIndex(counter).LBHash);
      ResNode.LBHash := GetHighest;
      End;
   // Get the consensus last block time end
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      AddValue(GetNodeIndex(counter).LBTimeEnd.ToString);
      ResNode.LBTimeEnd := GetHighest.ToInt64;
      End;
   // Get the consensus last block Miner
   SetLength(ArrT,0);
   For counter := 0 to LengthNodes-1 do
      Begin
      AddValue(GetNodeIndex(counter).LBMiner);
      ResNode.LBMiner := GetHighest;
      End;
   {More fields to get update}

   if ResNode.block>GetMainConsensus.block then
      begin
      result := true;
      SetMainConsensus(ResNode);
      end;
   end;
End;

Procedure LoadNodes();
var
  ThisNode : String  = '';
  Counter  : integer = 0;
  NewNode : TNodeData;
Begin
EnterCriticalSection(CS_NodesArray);
SetLength(NodesArray,0);
REPEAT
   Begin
   ThisNode := '';
   ThisNode := Parameter(DefaultNodes,counter);
   If ThisNode <> '' then
      begin
      NewNode := Default(TNodeData);
      ThisNode := StringReplace(ThisNode,':',' ',[rfReplaceAll, rfIgnoreCase]);
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

Function NosoHash(source:string):string;
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

Function CheckHashDiff(Target,ThisHash:String):string;
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

INITIALIZATION
InitCriticalSection(CS_NodesArray);
InitCriticalSection(CS_CSThread);
InitCriticalSection(CS_Consensus);


FINALIZATION
DoneCriticalSection(CS_NodesArray);
DoneCriticalSection(CS_CSThread);
DoneCriticalSection(CS_Consensus);

END. // End unit

