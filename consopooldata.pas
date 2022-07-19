UNIT consopooldata;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, coreunit, IdTCPServer, IdContext, IdGlobal, StrUtils,
  IdTCPClient, math, fileutil, nosodig.crypto;

Type

  ThreadPayment = class(TThread)
   private
     Address: string;
   protected
     procedure Execute; override;
   public
     constructor Create(const CreatePaused: Boolean; TAddress:string);
   end;

   ThreadUpdateBalance = class(TThread)
    protected
      procedure Execute; override;
    public
      Constructor Create(CreatePaused : boolean);
    end;

  PoolServerEvents = class
    class procedure OnExecute(AContext: TIdContext);
    class procedure OnConnect(AContext: TIdContext);
  end;

  TSolution   = Packed Record
    Hash    : string;
    address : string;
    Diff    : string;
    end;

   TMinersData = Packed Record
    address      : string[40];
    Balance      : int64;
    LastPay      : integer;
    Shares       : integer;
    LastPayOrder : String[120];
    end;

   TCounter = Packed record
    data         : string[60];
    counter      : integer;
    inblock      : integer;
    end;


Procedure AddPaymentToFile(Block,destino,monto,OrderID:String);
function SendOrder(OrderString:String):String;
function GetAddressBalance(Address:String):int64;

Procedure CreateMinersFile();
Procedure SaveMiners();
Procedure CreateBlockzero();
Procedure createPaymentsFile();
Procedure createNodesFile();
Function GetNodesFileData():String;
Procedure SaveMnsToDisk(lineText:string);
function GetMyLastUpdatedBlock():int64;
Procedure LoadMiners();
Function MinersCount():integer;
Function GetMinerBalance(address:string):int64;
Function GetMinerData(address:string):string;
Procedure ClearAddressBalance(Address, LastPayInfo:string);
Function GetTotalDebt():Int64;

Procedure AddShare(Share:string);
Function SharesCount():Integer;
Function ShareAlreadyExists(Share:string):boolean;
Procedure CreditShare(Address,IPUser:String);
Function ShareIsValid(Share,Address,MinerProgram,IPUser:String):integer;

Procedure SetSolution(Data:TSolution);
Function GetSolution():TSolution;

Function ResetLogs():boolean;
Procedure RawToLogFile(Linea:String);
function SaveConfig():boolean;
Procedure LoadConfig();
Function UpdateScreen():Boolean;
Procedure SetUpdateScreen();
Procedure InitServer();
function CheckIPMiners(UserIP:String):Boolean;
Function TryMessageToMiner(AContext: TIdContext;message:string):boolean;
Procedure TryClosePoolConnection(AContext: TIdContext; closemsg:string='');
Function StartPool():String;
Function UpTime():string;
Function StopPool():String;
Function GetDiffHashrate(bestdiff:String):integer;
Function GetSessionSpeed(): int64;
Procedure ToLog(Texto:string;ShowOnScreen:integer = 0);
Function GetBlockBest():String;
Procedure SetBlockBest(ThisValue:String);
Function DistributeBlockPayment():string;
Procedure RunPayments();
Procedure GenerateReport(destination:integer);
Procedure CounterReport(linea:String;Destination:integer);
Procedure BuildNewBlock();
Procedure ResetBlock();
Function GetPrefixIndex():Integer;
Procedure ResetPrefixIndex();
function GetPrefixStr(IndexValue:integer = -1):string;
Procedure SendSolution(Data:TSolution);

// Payment threads
Procedure SetPayThreads(Tvalue:integer);
Function GetPayThreads():integer;
Procedure DecreasePayThreads(WasGood:boolean);
// Pool Balance
Procedure SetPoolBalance(ThisValue:int64);
Function GetPoolBalance():Int64;
Procedure UpdatePoolBalance();
// LAstBlockRate
Procedure SetLastBlockRate(ThisValue:int64);
Function GetLastBlockRate():Int64;
// ShareIndex
Procedure SaveShareIndex();
Procedure LoadShareIndex();
Procedure CreditFrequency(Diff : string);
Procedure ShareIndexReport();
// Mainnet hashrate
Function GetBlockValue(Block:integer;valuename:string):string;
Procedure FillSolsArray();
Procedure CalculateMainNetHashrate();
Procedure GetBlocksMinedByPool();
// Debug Counters
Procedure AddUserMiner(Minerv:String);
Procedure ClearUserMinerArray(ClearAll:boolean = true);
Procedure AddUserIP(userip:string);
Procedure ClearUserIPArray(ClearAll:boolean = true);
Procedure AddShareIP(userip:string);
Procedure ClearShareIPArray(ClearAll:boolean = true);
Procedure AddWrongShareMiner(userip:string);
Procedure ClearWrongShareMiner(ClearAll:boolean = true);
Procedure AddWrongShareIp(ThisData:string);
Procedure ClearWrongShareIp(ClearAll:boolean = true);

CONST
  fpcVersion = {$I %FPCVERSION%};
  AppVersion = 'v0.44';
  DefHelpLine= 'Type help for available commands';
  DefWorst = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';

  // Predefined Types for OutPut
  uToBoth      = 0;
  uToFile      = 1;
  uToConsole   = 2;


VAR
  // Like a CONST
  StoreShares : boolean = False;
  // Counters
  UserMiner       : array of TCounter;
  UserIPArr       : array of TCounter;
  ShareIPArr      : array of TCounter;
  WrongShareMiner : array of TCounter;
  WrongShareIp    : array of TCounter;

  // Files
  configfile, LogFile, OldLogFile, PaysFile : TextFile;
  MinersFile : File of TMinersData;
  //TempMinersFile : File of TMinersData2;
  // Config values
  PoolPort     : integer = 8082;
  MinDiffBase  : string  = '00000';
  PoolFee      : integer = 100;
  PoolPay      : integer = 30;
  PoolAddress  : String  = '';
  PublicKey    : String  = '';
  PrivateKey   : String  = '';
  PoolAuto     : boolean = true;
  AutoDiff     : boolean = true;
  AutoValue    : integer = 500;
  // Operative
  previousNodes: string;
  ThisBlockMNs : boolean = false;
  ArraySols    : array of integer;
  ArrayMiner   : array of string;
  ShareIndex   : array[0..15] of integer;
  Command      : string  = '';
  LastCommand  : string  = '';
  ThisChar     : Char;
  LastHelpShown: String = '';
  FinishProgram: Boolean = false;
  OnMainScreen : Boolean = True;
  OnLogScreen  : Boolean = false;
    RefreshScreen : Boolean = true;
    RefreshAge    : Int64 = 0;
    UpdateServerInfo : boolean = false;
    RefreshUpTime : int64 = 0;
    RefreshPoolBalance : boolean = false;
    RefreshPoolHeader  : boolean = false;
  LogLines     : Array of String;
  NewLogLines  : Array of string;
  MainnetTimeStamp : int64 = 0;
  RejectedShares   : integer = 0;
  RestartAfterQuit : boolean = false;
  FileToRestart    : string = '';
  // Mainnet
  LastConsensusTry : int64   = 0;
  WaitingConsensus :Boolean = false;
  CurrentBlock     : integer = 0;
  MainBestDiff     : String = DefWorst;
  MainnetHashRate  : int64 = 0;
  BlocksMinedByPool: integer = 0;
  // Pool
  PoolBalance      : int64 = 0;
  CheckPaysThreads : Boolean = false;
  OpenPayThreads : integer = 0;
    GoodPayments : integer = 0;
    BadPayments  : integer = 0;
  LastPaidBlock : integer = 0;
  LastBlockRate : int64 = 0;
  PoolServer : TIdTCPServer;
  PrefixIndex: Integer = 0;
  MinerDiff  : String = '';
  IPMiners: integer = 100;
  ArrMiners : Array of TMinersData;
  ArrShares : Array of string;
  BlockTargetHash : String = '';
  ThisBlockBest   : String = DefWorst;
  Solution        : String = '';
  BlockPrefixesRequested : integer = 0;
  BestPoolSolution: TSolution;
  SESSION_BestHashes : Integer = 0;
  SESSION_Started    : Int64 = 0;
  SESSION_Shares     : integer = 0;
  SESSION_HashPerShare : QWord = 0;
  // Critical sections
  CS_UpdateScreen    : TRTLCriticalSection;
  CS_PrefixIndex     : TRTLCriticalSection;
  CS_LogLines        : TRTLCriticalSection;
  CS_Miners          : TRTLCriticalSection;
  CS_Shares          : TRTLCriticalSection;
  CS_BlockBest       : TRTLCriticalSection;
  CS_Solution        : TRTLCriticalSection;
  CS_PaysFile        : TRTLCriticalSection;
  CS_PayThreads      : TRTLCriticalSection;
  CS_PoolBalance     : TRTLCriticalSection;
  CS_LastBlockRate   : TRTLCriticalSection;
  CS_USerMiner       : TRTLCriticalSection;
  CS_UserIPArr       : TRTLCriticalSection;
  CS_ShareIPArr      : TRTLCriticalSection;
  CS_WrongShareMiner : TRTLCriticalSection;
  CS_WrongShareIp    : TRTLCriticalSection;

IMPLEMENTATION

constructor ThreadUpdateBalance.Create(CreatePaused : boolean);
Begin
inherited Create(CreatePaused);
FreeOnTerminate := True;
End;

procedure ThreadUpdateBalance.Execute;
var
  ThisValue : int64 = 0;
Begin
ThisValue := GetAddressBalance(PoolAddress);
SetPoolBalance(ThisValue);
RefreshPoolBalance := true;
End;

constructor ThreadPayment.Create(const CreatePaused: Boolean; TAddress:string);
Begin
inherited Create(CreatePaused);
Address := TAddress;
FreeOnTerminate := True;
End;

procedure ThreadPayment.Execute;
var
  Balance    : int64;
  Fee        : int64;
  ToSend     : int64;
  TrxTime    : int64;
  trfHash    : String;
  OrdHash    : String;
  SignString : String;
  TextToSend : string;
  Resultado  : String = '';
  WasGood    : boolean = false;
  LastPayInfo: string = '';
  ErrorCode  : integer = 0;
Begin
Balance := GetMinerBalance(address);
Fee     := GetFee(Balance);
ToSend  := Balance - Fee;
if ToSend<=0 then
   begin
   LastPayInfo := (GetMainConsensus.block+1).ToString+':'+ToSend.ToString+':'+'TooSmallAmount';
   ClearAddressBalance(Address, LastPayInfo);
   AddPaymentToFile((GetMainConsensus.block+1).ToString,Address,Balance.ToString,Resultado);
   WasGood := true;
   DecreasePayThreads(WasGood);
   exit;
   end;
if not IsValidHashAddress(Address) then
   begin
   LastPayInfo := (GetMainConsensus.block+1).ToString+':'+ToSend.ToString+':'+'InvalidAddress';
   ClearAddressBalance(Address, LastPayInfo);
   AddPaymentToFile((GetMainConsensus.block+1).ToString,Address,Balance.ToString,Resultado);
   WasGood := true;
   DecreasePayThreads(WasGood);
   exit;
   end;
TrxTime := UTCTime;
trfHash := GetTransferHash(TrxTime.ToString+PoolAddress+address+ToSend.ToString+IntToStr(GetMainConsensus.block));
OrdHash := GetOrderHash('1'+TrxTime.ToString+trfHash);

SignString := GetStringSigned(TrxTime.ToString+PoolAddress+Address+ToSend.ToString+
                     Fee.ToString+'1',PrivateKey);

TextToSend := 'NSLORDER 1 0.16 '+TrxTime.ToString+' ORDER 1 $TRFR '+OrdHash+' 1 TRFR '+TrxTime.ToString+
              ' Pool_Payment 1 '+PublicKey+' '+PoolAddress+' '+Address+' '+Fee.ToString+' '+ToSend.ToString+' '+
              SignString+' '+trfHash;
Resultado  := SendOrder(TextToSend);
if ( (Resultado <> '') and (Parameter(Resultado,0)<>'ERROR') and(Resultado<>'Closing NODE') ) then
   begin
   LastPayInfo := (GetMainConsensus.block+1).ToString+':'+ToSend.ToString+':'+Resultado;
   ClearAddressBalance(Address, LastPayInfo);
   AddPaymentToFile((GetMainConsensus.block+1).ToString,Address,Balance.ToString,Resultado);
   WasGood := true;
   end
else
   begin
   ErrorCode := StrToIntDef(Parameter(Resultado,0),-1);
   ToLog(' Payment rejected by mainnet error: '+ErrorCode.ToString,uToFile);
   end;
DecreasePayThreads(WasGood);
End;

Procedure AddPaymentToFile(Block,destino,monto,OrderID:String);
var
  ThisFile : TextFile;
Begin
EnterCriticalSection(CS_PaysFile);
Append(PaysFile);
Writeln(PaysFile,Format('%s %s %s %s',[block,destino,monto,OrderID]));
CloseFile(PaysFile);
LeaveCriticalSection(CS_PaysFile);
AssignFile(ThisFile,'addresses'+directoryseparator+destino+'.txt');
TRY
if not fileExists('addresses'+directoryseparator+destino+'.txt') then rewrite(ThisFile)
else Append(ThisFile);
Writeln(ThisFile,Format('%s %s %s',[block,monto,OrderID]));
CloseFile(ThisFile);
EXCEPT on E:Exception do
   begin
   ToLog(' Error saving payment to address file: '+destino+'->'+E.Message);
   end;
END{Try};
End;

// Sends a order to the mainnet
function SendOrder(OrderString:String):String;
var
  Client : TidTCPClient;
  RanNode : integer;
  ThisNode : TNodeData;
  Trys : integer = 0;
  WasDone : Boolean = false;
Begin
Result := '';
REPEAT
   RanNode := Random(LengthNodes);
   ThisNode := GetNodeIndex(RanNode);
   Client := TidTCPClient.Create(nil);
   Client.Host:=ThisNode.host;
   Client.Port:=ThisNode.port;
   Client.ConnectTimeout:= 3000;
   Client.ReadTimeout:=3000;

   TRY
   Client.Connect;
   Client.IOHandler.WriteLn(OrderString);
   Result := Client.IOHandler.ReadLn(IndyTextEncoding_UTF8);
   WasDone := True;
   EXCEPT on E:Exception do
      begin
      WasDone := False;
      end;
   END{Try};
Inc(Trys);
UNTIL ( (WasDone) or (Trys=3) );
if client.Connected then Client.Disconnect();
client.Free;
End;

function GetAddressBalance(Address:String):int64;
var
  Client : TidTCPClient;
  RanNode : integer;
  ThisNode : TNodeData;
  Trys : integer = 0;
  WasDone : boolean = false;
Begin
Result := 0;
REPEAT
   RanNode := Random(LengthNodes);
   ThisNode := GetNodeIndex(RanNode);
   Client := TidTCPClient.Create(nil);
   Client.Host:=ThisNode.host;
   Client.Port:=ThisNode.port;
   Client.ConnectTimeout:= 1000;
   Client.ReadTimeout:=800;
   TRY
   Client.Connect;
   Client.IOHandler.WriteLn('NSLBALANCE '+Address);
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

Procedure CreateMinersFile();
Begin
TRY
rewrite(MinersFile);
CloseFile(MinersFile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error creating miners file');
   Halt(1);
   end;
END {TRY};
End;

Procedure SaveMiners();
var
  Counter : integer;
  AlreadyAdded : string = '';
  //ThisTemp : TMinersData2;
Begin
EnterCriticalSection(CS_Miners);
TRY
rewrite(MinersFile);
for counter := 0 to length(ArrMiners)-1 do
   begin
   if ( (ArrMiners[counter].Shares>0) or (ArrMiners[counter].Balance>0) or((ArrMiners[counter].LastPay+36)<GetMAinConsensus.block) ) then
      begin
      if not AnsiContainsStr(AlreadyAdded,ArrMiners[counter].address) then
         begin
         write(MinersFile,ArrMiners[counter]);
         AlreadyAdded := AlreadyAdded+ArrMiners[counter].address+',';
         end;
      end;
   end;
//CloseFile(TempMinersFile);
CloseFile(MinersFile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error saving miners file');
   readln();
   Halt(1);
   end;
END {TRY};
LeaveCriticalSection(CS_Miners);
LoadMiners;
End;

Procedure CreateBlockzero();
var
  ThisFile : TextFile;
Begin
AssignFile(ThisFile,'blocks'+DirectorySeparator+'0.txt');
TRY
rewrite(ThisFile);
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error creating block 0');
   Halt(1);
   end;
END {TRY};
End;

Procedure createPaymentsFile();
var
  ThisFile : TextFile;
Begin
AssignFile(ThisFile,'payments.txt');
TRY
rewrite(ThisFile);
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error creating payments file');
   Halt(1);
   end;
END {TRY};
End;

Procedure createNodesFile();
var
  ThisFile : TextFile;
Begin
AssignFile(ThisFile,'nodes.txt');
TRY
rewrite(ThisFile);
write(ThisFile,'0 109.230.238.240;8080:N3iEmfEoYhW99Gn6U6EaLfJ3bqmWCD3:114 '+
                      '198.144.190.194;8080:N4DixvMj1ZEBhm1xbxmCNursoZxPeH1:114 '+
                      '107.175.59.177;8080:N4VJxLRtbvngmThBJohq7aHd5BwKbFf:76 '+
                      '107.172.193.176;8080:N3sb23UXr23Som3B11u5q7qR9FvsDC7:114 '+
                      '66.151.117.247;8080:NUhcAdqnVDHtd8NmMMo6sLK3bmYFE5:56 '+
                      '192.3.73.184;8080:N2RJi7FYf76UBH9RyhndTofskzHKuEe:114 '+
                      '107.175.24.151;8080:N4HrfiM6YVw2g4oAmWGKCvU5PXpZ2DM:126 '+
                      '149.57.137.108;8080:N46PiNk7chSURJJZoMSRdwsDh8FAbDa:114 '+
                      '3.111.137.132;58445:N4PeJyqj8diSXnfhxSQdLpo8ddXTaGd:176');
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error creating nodes file');
   Halt(1);
   end;
END {TRY};
previousNodes := '0 109.230.238.240;8080:N3iEmfEoYhW99Gn6U6EaLfJ3bqmWCD3:114 '+
                      '198.144.190.194;8080:N4DixvMj1ZEBhm1xbxmCNursoZxPeH1:114 '+
                      '107.175.59.177;8080:N4VJxLRtbvngmThBJohq7aHd5BwKbFf:76 '+
                      '107.172.193.176;8080:N3sb23UXr23Som3B11u5q7qR9FvsDC7:114 '+
                      '66.151.117.247;8080:NUhcAdqnVDHtd8NmMMo6sLK3bmYFE5:56 '+
                      '192.3.73.184;8080:N2RJi7FYf76UBH9RyhndTofskzHKuEe:114 '+
                      '107.175.24.151;8080:N4HrfiM6YVw2g4oAmWGKCvU5PXpZ2DM:126 '+
                      '149.57.137.108;8080:N46PiNk7chSURJJZoMSRdwsDh8FAbDa:114 '+
                      '3.111.137.132;58445:N4PeJyqj8diSXnfhxSQdLpo8ddXTaGd:176';
End;

Function GetNodesFileData():String;
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

Procedure SaveMnsToDisk(lineText:string);
var
  ThisFile : TextFile;
Begin
if LineText = '' then exit;
AssignFile(ThisFile,'nodes.txt');
TRY
rewrite(ThisFile);
write(ThisFile,lineText);
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION do
   begin

   end;
END {TRY};
End;

// Returns the last processed block
function GetMyLastUpdatedBlock():int64;
Var
  BlockFiles : TStringList;
  contador : int64 = 0;
  LastBlock : int64 = 0;
  OnlyNumbers : String;
Begin
BlockFiles := TStringList.Create;
   TRY
   FindAllFiles(BlockFiles, 'blocks'+DirectorySeparator, '*.txt', true);
   while contador < BlockFiles.Count do
      begin
      OnlyNumbers := copy(BlockFiles[contador], 8, length(BlockFiles[contador])-11);
      LastBlock := StrToIntDef(OnlyNumbers,-1);
      contador := contador+1;
      end;
   Result := LastBlock;
   EXCEPT on E:Exception do
      tolog (' Error getting my last updated block');
   END; {TRY}
BlockFiles.Free;
end;

Procedure LoadMiners();
var
  ThisData : TMinersData;
Begin
reset(MinersFile);
EnterCriticalSection(CS_Miners);
SetLength(ArrMiners,0);
While not eof(MinersFile) do
   begin
   read(MinersFile,ThisData);
   Insert(ThisData,ArrMiners,Length(ArrMiners));
   end;
LeaveCriticalSection(CS_Miners);
CloseFile(MinersFile);
End;

Function MinersCount():Integer;
Begin
EnterCriticalSection(CS_Miners);
Result := Length(ArrMiners);
LeaveCriticalSection(CS_Miners);
End;

Function GetMinerBalance(address:string):int64;
var
  counter : integer;
Begin
Result := 0;
EnterCriticalSection(CS_Miners);
for counter := 0 to length(ArrMiners)-1 do
   begin
   if ArrMiners[counter].address = address then
      begin
      result := ArrMiners[counter].Balance;
      break;
      end;
   end;
LeaveCriticalSection(CS_Miners);
End;

Function GetMinerData(address:string):string;
var
  counter    : integer;
  BlocksTill : integer;
Begin
Result := '';
EnterCriticalSection(CS_Miners);
for counter := 0 to length(ArrMiners)-1 do
   begin
   if ArrMiners[counter].address = address then
      begin
      BlocksTill := PoolPay-(GetMainConsensus.block-ArrMiners[counter].LastPay);
      If BlocksTill<0 then BlocksTill := 0;
      result := Format('%d %d %s',[ArrMiners[counter].Balance,BlocksTill,ArrMiners[counter].LastPayOrder]);
      break;
      end;
   end;
LeaveCriticalSection(CS_Miners);
End;

Procedure ClearAddressBalance(Address, LastPayInfo:string);
var
  counter : integer;
Begin
EnterCriticalSection(CS_Miners);
for counter := 0 to length(ArrMiners)-1 do
   begin
   if ArrMiners[counter].address = address then
      begin
      ArrMiners[counter].Balance := 0;
      ArrMiners[counter].LastPay:=GetMAinConsensus.block;
      ArrMiners[counter].LastPayOrder:=LastPayInfo;
      break;
      end;
   end;
LeaveCriticalSection(CS_Miners);
End;

Function GetTotalDebt():Int64;
var
  counter : integer;
Begin
result := 0;
EnterCriticalSection(CS_Miners);
for counter := 0 to length(ArrMiners)-1 do
   begin
   result := result + ArrMiners[counter].Balance;
   end;
LeaveCriticalSection(CS_Miners);
End;

Function ShareAlreadyExists(Share:string):boolean;
var
  counter : integer;
Begin
Result := false;
EnterCriticalSection(CS_Shares);
For counter := 0 to length(ArrShares)-1 do
   begin
   if ArrShares[counter] = Share then
      begin
      result := true;
      break
      end;
   end;
LeaveCriticalSection(CS_Shares);
End;

Procedure CreditShare(Address,IPUser:String);
var
  counter : integer;
  Credited : boolean = false;
  NewMiner : TMinersData;
Begin
EnterCriticalSection(CS_Miners);
For counter := 0 to length(ArrMiners)-1 do
   begin
   if ArrMiners[counter].address = address then
      begin
      Credited := true;
      ArrMiners[counter].Shares:=ArrMiners[counter].Shares+1;
      break
      end;
   end;
if Not Credited then
   begin
   NewMiner := Default(TMinersData);
   NewMiner.address:=address;
   NewMiner.Shares:=1;
   NewMiner.Balance:=0;
   NewMiner.LastPay:=GetMainConsensus.block;
   Insert(NewMiner,ArrMiners,Length(ArrMiners));
   end;
LeaveCriticalSection(CS_Miners);
AddShareIP(IPUser);
End;

Procedure AddShare(Share:string);
Begin
EnterCriticalSection(CS_Shares);
insert(share,ArrShares,length(ArrShares));
LeaveCriticalSection(CS_Shares);
End;

Function SharesCount():Integer;
Begin
EnterCriticalSection(CS_Shares);
Result := length(ArrShares);
LeaveCriticalSection(CS_Shares);
End;

Procedure SetSolution(Data:TSolution);
Begin
EnterCriticalSection(CS_Solution);
BestPoolSolution := Data;
if BestPoolSolution.Diff = '' then BestPoolSolution.Diff := 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
LeaveCriticalSection(CS_Solution);
End;

Function GetSolution():TSolution;
Begin
EnterCriticalSection(CS_Solution);
Result := BestPoolSolution;
LeaveCriticalSection(CS_Solution);
End;

Function ShareIsValid(Share,Address,MinerProgram,IPUser:String):integer;
var
  ThisHash, ThisDiff : string;
  ThisSolution : TSolution;
Begin
Result := 0;
if ShareAlreadyExists(Share) then
   begin
   ToLog(Format(' Error 4 [%s] [%s]',[MinerProgram,IPUser]),uToFile);
   result := 4;
   Inc(RejectedShares);
   AddWrongShareMiner(MinerProgram);
   AddWrongShareIp(IPUser);
   end
else if ((length(Share)<18) or (length(Share)>33)) then
   begin
   ToLog(Format(' Error 7 [%s] [%s]',[MinerProgram,IPUser]),uToFile);
   result := 7;
   Inc(RejectedShares);
   AddWrongShareMiner(MinerProgram);
   AddWrongShareIp(IPUser);
   end
else
   begin
   ThisHash := NosoHash(Share+PoolAddress);
   ThisDiff := GetHashDiff(GetMainConsensus.LBHash,ThisHash);
   if ThisDiff < MinerDiff then // Valid share
      begin
      Result := 0;
      AddShare(Share+Address);
      CreditShare(Address,IPUser);
      If StoreShares then CreditFrequency(ThisDiff);
      Inc(SESSION_Shares);
      //UpdateServerInfo := true;
      if ThisDiff<MainBestDiff then
         begin
         ThisSolution.Hash:=Share;
         ThisSolution.address:=Address;
         ThisSolution.Diff:=ThisDiff;
         SetSolution(ThisSolution);
         SetBlockBest(ThisDiff);
         end;
      if ThisDiff<GetBlockBEst then
         begin
         ThisSolution.Hash:=Share;
         ThisSolution.address:=Address;
         ThisSolution.Diff:=ThisDiff;
         SetBlockBest(ThisDiff)
         end;
      end
   else
      begin
      result := 5;
      ToLog(Format(' Error 5 [%s] [%s]',[MinerProgram,IPUser]),uToFile);
      Inc(RejectedShares);
      AddWrongShareMiner(MinerProgram);
      AddWrongShareIp(IPUser);
      end;
   end;
End;

// Prepare logs files
Function ResetLogs():boolean;
var
  ThisLine : string;
Begin
result := true;
TRY
If not FileExists('logs'+DirectorySeparator+'oldlogs.txt') then
   Begin
   Rewrite(OldLogFile);
   CloseFile(OldLogFile);
   end;
If not FileExists('logs'+DirectorySeparator+'log.txt') then
   Begin
   Rewrite(logfile);
   CloseFile(logfile);
   end;
Reset(logfile);
Append(OldLogFile);
While not Eof(LogFile) do
   begin
   ReadLn(LogFile,ThisLine);
   WriteLn(OldLogFile,ThisLine);
   end;
CloseFile(LogFile);
CloseFile(OldLogFile);
Rewrite(LogFile);
CloseFile(LogFile);
EXCEPT ON E:Exception do
   begin
   Result := false;
   ToLog(' RESETLOGS: '+E.Message);
   end;
END {Try};
End;

Procedure RawToLogFile(Linea:String);
Begin
TRY
append(logfile);
WriteLn(LogFile,Linea);
CloseFile(LogFile);
EXCEPT ON E:EXCEPTION do
   begin
   end;
END {TRY};
End;

// Creates/saves config data
Function SaveConfig():boolean;
Begin
result := true;
TRY
rewrite(configfile);
writeln(configfile,'pooladdress '+PoolAddress);
writeln(configfile,'publickey '+PublicKey);
writeln(configfile,'privatekey '+PrivateKey);
writeln(configfile,'poolport '+IntToStr(PoolPort));
writeln(configfile,'diffbase '+MinDiffBase);
writeln(configfile,'poolfee '+IntToStr(PoolFee));
writeln(configfile,'poolpay '+IntToStr(PoolPay));
writeln(configfile,'ipminers '+IntToStr(IPMiners));
writeln(configfile,'autostart '+BoolToStr(PoolAuto,True));
writeln(configfile,'autodiff '+BoolToStr(AutoDiff,True));
writeln(configfile,'autovalue '+IntToStr(AutoValue));

CloseFile(configfile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error saving config file: '+E.Message);
   end
END {TRY};
End;

Procedure LoadConfig();
var
  linea : string;
Begin
TRY
reset(configfile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error opening config file: '+E.Message);
   exit
   end
END {TRY};
TRY
while not eof(configfile) do
   begin
   readln(configfile,linea);
   if uppercase(Parameter(linea,0)) = 'POOLPORT' then PoolPort := StrToIntDef(Parameter(linea,1),Poolport);
   if uppercase(Parameter(linea,0)) = 'DIFFBASE' then MinDiffBase := Parameter(linea,1);
   if uppercase(Parameter(linea,0)) = 'POOLFEE' then PoolFee := StrToIntDef(Parameter(linea,1),PoolFee);
   if uppercase(Parameter(linea,0)) = 'POOLPAY' then PoolPay := StrToIntDef(Parameter(linea,1),PoolPay);
   if uppercase(Parameter(linea,0)) = 'POOLADDRESS' then PoolAddress := Parameter(linea,1);
   if uppercase(Parameter(linea,0)) = 'PUBLICKEY' then PublicKey := Parameter(linea,1);
   if uppercase(Parameter(linea,0)) = 'PRIVATEKEY' then PrivateKey := Parameter(linea,1);
   if uppercase(Parameter(linea,0)) = 'IPMINERS' then IPMiners := StrToIntDef(Parameter(linea,1),IPMiners);
   if uppercase(Parameter(linea,0)) = 'AUTOSTART' then PoolAuto := StrToBool(Parameter(linea,1));
   if uppercase(Parameter(linea,0)) = 'AUTODIFF' then AutoDiff := StrToBool(Parameter(linea,1));
   if uppercase(Parameter(linea,0)) = 'AUTOVALUE' then AutoValue := StrToIntDef(Parameter(linea,1),AutoValue);
   end;
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error reading config file: '+E.Message);
   exit
   end
END {TRY};
TRY
CloseFile(configfile);
EXCEPT ON E:EXCEPTION do
   begin
   writeln('Error closing config file: '+E.Message);
   exit
   end
END {TRY};
End;

Function UpdateScreen():Boolean;
Begin
EnterCriticalSection(CS_UpdateScreen);
Result := RefreshScreen;
RefreshScreen := false;
LeaveCriticalSection(CS_UpdateScreen);
End;

Procedure SetUpdateScreen();
Begin
EnterCriticalSection(CS_UpdateScreen);
RefreshScreen := True;
LeaveCriticalSection(CS_UpdateScreen);
End;

Procedure InitServer();
Begin
PoolServer := TIdTCPServer.Create(nil);
PoolServer.DefaultPort:=PoolPort;
PoolServer.Active:=false;
PoolServer.UseNagle:=true;
PoolServer.TerminateWaitTime:=5000;
PoolServer.OnExecute:=@PoolServerEvents.OnExecute;
PoolServer.OnConnect:=@PoolServerEvents.OnConnect;
End;

Function StartPool():String;
Var
  Success: boolean = false;
Begin
if PoolServer.Active then
   Begin
   Result := 'Pool already active';
   exit;
   end;
TRY
PoolServer.Bindings.Clear;
PoolServer.DefaultPort:=PoolPort;
PoolServer.Active:=true;
Success := true;
MinerDiff := AddCharR('F',MinDiffBase,32);
ToLog(' Server started at port '+PoolPort.ToString);
SESSION_Started := UTCTime;
SESSION_HashPerShare := Round(Power(16,GetDiffHashrate(MinerDiff)/100));
ToLog(' Hashes Per Share : '+SESSION_HashPerShare.ToString);
if Offset <> 0 then ToLog('.Time offset : '+Offset.ToString);
EXCEPT ON E:Exception do
   Result := E.Message;
END; {TRY}
If success then
   Begin
   Result := 'Pool server started';
   UpdateServerInfo := true;
   end;
End;

Function UpTime():string;
var
  TotalSeconds,days,hours,minutes,seconds, remain : integer;
Begin
if SESSION_Started = 0 then
   begin
   Result := '00:00:00';
   exit;
   end;
Totalseconds := UTCTime-SESSION_Started;
Days := Totalseconds div 86400;
remain := Totalseconds mod 86400;
hours := remain div 3600;
remain := remain mod 3600;
minutes := remain div 60;
remain := remain mod 60;
seconds := remain;
if Days > 0 then Result:= Format('%dd %.2d:%.2d:%.2d', [Days, Hours, Minutes, Seconds])
else Result:= Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
End;

Function StopPool():String;
Var
  Success: boolean = false;
Begin
if not PoolServer.Active then
   Begin
   Result := 'Pool is already unactive';
   exit;
   end;
TRY
PoolServer.Active:=false;
Success := true;
SESSION_Started := 0;
EXCEPT On E:Exception do
   Result := E.Message;
END; {TRY}
If success then
   Begin
   Result := 'Pool stopped';
   UpdateServerInfo := true;
   end;
End;

Function GetDiffHashrate(bestdiff:String):integer;
var
  counter:integer = 0;
Begin
repeat
  counter := counter+1;
until bestdiff[counter]<> '0';
Result := (Counter-1)*100;
if bestdiff[counter]='1' then Result := Result+50;
if bestdiff[counter]='2' then Result := Result+25;
if bestdiff[counter]='3' then Result := Result+12;
if bestdiff[counter]='4' then Result := Result+6;
End;

Function GetSessionSpeed(): int64;
var
  seconds : integer;
Begin
Seconds := UTCTime-SESSION_Started;
Result := (SESSION_HashPerShare*SESSION_Shares) div (seconds+1);
End;

Procedure ToLog(Texto:string;ShowOnScreen:integer = 0);
Begin
if ShowOnScreen = uToFile then
   begin
   Texto := copy(Texto,1,1)+TimeToStr(now)+' '+copy(Texto,2, length(texto));
   RawToLogFile(copy(Texto,2, length(texto)))
   end
else if ShowOnScreen = uToConsole then
   begin
   EnterCriticalSection(CS_LogLines);
   Insert(Texto,LogLines,Length(LogLines));
   LeaveCriticalSection(CS_LogLines);
   end
else
   begin
   Texto := copy(Texto,1,1)+TimeToStr(now)+' '+copy(Texto,2, length(texto));
   RawToLogFile(copy(Texto,2, length(texto)));
   EnterCriticalSection(CS_LogLines);
   Insert(Texto,LogLines,Length(LogLines));
   LeaveCriticalSection(CS_LogLines);
   end;
End;

Function GetBlockBest():String;
Begin
EnterCriticalSection(CS_BlockBest);
Result := ThisBlockBest;
LeaveCriticalSection(CS_BlockBest);
End;

Procedure SetBlockBest(ThisValue:String);
Begin
EnterCriticalSection(CS_BlockBest);
ThisBlockBest := ThisValue;
LeaveCriticalSection(CS_BlockBest);
End;

Function DistributeBlockPayment():string;
var
  counter      : integer;
  TotalShares  : Integer = 0;
  ToDistribute : int64;
  PerShare     : int64;
  Comision     : int64;
  Earned       : int64;
Begin
Result := '';
ToDistribute := GetMainConsensus.LBPoW;
Comision := (ToDistribute * PoolFee) div 10000;
ToDistribute := ToDistribute - Comision;
EnterCriticalSection(CS_Miners);
For counter := 0 to length(ArrMiners)-1 do
   TotalShares := TotalShares+ArrMiners[counter].Shares;
PerShare := ToDistribute div TotalShares;
For counter := 0 to length(ArrMiners)-1 do
   begin
   if ArrMiners[counter].Shares>0 then
      begin
      ArrMiners[counter].Balance:=ArrMiners[counter].Balance+((ArrMiners[counter].Shares * PerShare));
      ArrMiners[counter].Shares := 0;
      end;
   end;
LeaveCriticalSection(CS_Miners);
Earned := GetMainConsensus.LBPoW-(PerShare*TotalShares);
Result := PerShare.ToString+' '+Earned.ToString;
End;

Procedure RunPayments();
var
  ThisBlock, counter : integer;
  ThisThread : ThreadPayment;
  CopyArray : Array of TMinersData ;
  PayingAddresses : integer = 0;
  TotalToPay : int64 = 0;
Begin
ThisBlock := GetMainConsensus.block;
SetLength(CopyArray,0);
EnterCriticalSection(CS_Miners);
CopyArray := copy(ArrMiners,0,length(ArrMiners));
LeaveCriticalSection(CS_Miners);
For counter := 0 to length(CopyArray)-1 do
   begin
   if ((CopyArray[counter].Balance>0)and(CopyArray[counter].LastPay+30<= ThisBlock)and(CopyArray[counter].address<>PoolAddress) )then
      begin
      PayingAddresses := PayingAddresses+1;
      TotalToPay := TotalToPay + CopyArray[counter].Balance;
      ThisThread := ThreadPayment.create(true,CopyArray[counter].address);
      ThisThread.FreeOnTerminate:=true;
      ThisThread.Start;
      end;
   if ((CopyArray[counter].Balance>0)and(CopyArray[counter].LastPay+30<= ThisBlock)and(CopyArray[counter].address=PoolAddress) )then
      begin
      // Same address than pool address, do not send to save fees.
      ClearAddressBalance(CopyArray[counter].address,ThisBlock.ToString+':'+CopyArray[counter].Balance.ToString+':'+'OwnPayment');
      end;
   end;
if PayingAddresses>0 then
   begin
   ToLog(Format(' Paying to %d addresses : %s',[PayingAddresses,Int2Curr(TotalToPay)]));
   SetPayThreads(PayingAddresses);
   CheckPaysThreads := true;
   end
else
   begin
   SaveMiners();
   GenerateReport(uToFile);
   UpdatePoolBalance;
   end;
End;

Procedure GenerateReport(destination:integer);
var
  CopyArray, finalArray : Array of TMinersData ;
  counter, counter2   : integer;
  Inserted : boolean;
  ReportFile : TextFile;
  BalanceTotal : int64 = 0;
  SharesTotal  : int64 = 0;
  AddText,BalanText, sharestext, ToPayText : string;
Begin
SetLength(CopyArray,0);
SetLength(finalArray,0);
EnterCriticalSection(CS_Miners);
CopyArray := copy(ArrMiners,0,length(ArrMiners));
LeaveCriticalSection(CS_Miners);
for counter := 0 to length(CopyArray)-1 do
   begin
   if ( (CopyArray[counter].Balance>0) or (CopyArray[counter].Shares>0) ) then
      begin
      BalanceTotal := BalanceTotal+ CopyArray[counter].Balance;
      SharesTotal  := SharesTotal + CopyArray[counter].Shares;
      Inserted := false;
      for counter2 := 0 to length(finalArray)-1 do
         begin
         if CopyArray[counter].Balance>FinalArray[counter2].Balance then
            begin
            insert(CopyArray[counter],FinalArray,counter2);
            Inserted := true;
            break;
            end;
         end;
      if not inserted then Insert(CopyArray[counter],FinalArray,Length(FinalArray));
      end;
   end;
if Destination = uToFile then
   begin
   AssignFile(ReportFile,'report.txt');
   Rewrite(ReportFile);
   writeln(ReportFile,'Block : '+GetMAinConsensus.block.ToString());
   writeln(ReportFile,'Miners: '+Length(FinalArray).ToString());
   WriteLn(ReportFile,'Debt  : '+Int2Curr(BalanceTotal));
   WriteLn(ReportFile,'Shares: '+SharesTotal.ToString);
   Writeln(ReportFile,'');
   for counter2 := 0 to length(finalArray)-1 do
      begin
      AddText   := format('%0:-35s',[FinalArray[counter2].address]);
      BalanText := Format('%0:12s',[Int2Curr(FinalArray[counter2].balance)]);
      sharestext:= Format('%0:5s',[FinalArray[counter2].Shares.ToString]);
      writeln(ReportFile,format('%s %s %s',[AddText,BalanText,sharestext]));
      end;
   CloseFile(ReportFile);
   ToLog(format('/Report file Generated [ %s ]',[Int2Curr(GetAddressBalance(PoolAddress)-BalanceTotal)]));
   end
else if Destination = uToConsole then
   Begin
   ToLog(',Report',uToConsole);
   ToLog(' Block : '+GetMAinConsensus.block.ToString(),uToConsole);
   ToLog(' Miners: '+Length(FinalArray).ToString(),uToConsole);
   ToLog(' Debt  : '+Int2Curr(BalanceTotal),uToConsole);
   ToLog(' Shares: '+SharesTotal.ToString,uToConsole);
   AddText   := format('%0:-35s',['Address']);
   BalanText := Format('%0:12s',['Balance  ']);
   sharestext:= Format('%0:6s',['Shares']);
   ToPayText := Format('%0:6s',['To pay']);
   ToLog('/----------------------------------------------------------------------',uToConsole);
   ToLog(format('/%s | %s | %s | %s |',[AddText,BalanText,sharestext, ToPayText]),uToConsole);
   ToLog('/----------------------------------------------------------------------',uToConsole);
   for counter2 := 0 to length(finalArray)-1 do
      begin
      AddText   := format('%0:-35s',[FinalArray[counter2].address]);
      BalanText := Format('%0:12s',[Int2Curr(FinalArray[counter2].balance)]);
      sharestext:= Format('%0:6s',[FinalArray[counter2].Shares.ToString]);
      ToPayText := Format('%0:6d',[FinalArray[counter2].LastPay+30-GetMainConsensus.block]);
      ToLog(format(' %s | %s | %s | %s |',[AddText,BalanText,sharestext, ToPayText]),uToConsole);
      end;
   ToLog('/----------------------------------------------------------------------',uToConsole);
   end;
End;

Procedure CounterReport(Linea:String;Destination:integer);
var
  counter, count2 : integer;
  copyArray,FinalArray : array of TCOunter;
  MinerApp, thisCounter, ThisinBlock  : String;
  ReportTitle, dataname : string;
  TotalRecords : integer = 0;
  Added : boolean;
  ReportType : string = '';
  NumberRecords : integer = 0;
  SubString  : string;
Begin
ReportType := Parameter(Linea,1);
if Pos(' -n:',linea) > 0 then
   begin
   substring := copy(linea,Pos(' -n:',linea)+4,length(linea));
   NumberRecords := StrToIntDef(Parameter(substring,0),0);
   end
else if Pos(' -r',linea) > 0 then
   begin
   if Uppercase(ReportType) = 'MINERS' then ClearUserMinerArray
   else if Uppercase(ReportType) = 'IPS' then ClearUserIPArray
   else if Uppercase(ReportType) = 'SHAREIP' then ClearShareIPArray
   else if Uppercase(ReportType) = 'WRONGMINER' then ClearWrongShareMiner
   else if Uppercase(ReportType) = 'WRONGIP' then ClearWrongShareIp
   else ToLog('.Unknown report type: '+ReportType,uToConsole);
   exit;
   end;
if Uppercase(ReportType) = 'MINERS' then
   begin
   ReportTitle := 'Miners Report';
   Dataname    := 'MinerApp';
   SetLength(CopyArray,0);
   EnterCriticalSection(CS_USerMiner);
   CopyArray := Copy(UserMiner,0,length(UserMiner));
   LeaveCriticalSection(CS_USerMiner);
   end
else if Uppercase(ReportType) = 'IPS' then
   begin
   ReportTitle := 'Users IP Report';
   Dataname    := 'IP';
   SetLength(CopyArray,0);
   EnterCriticalSection(CS_UserIPArr);
   CopyArray := Copy(UserIPArr,0,length(UserIPArr));
   LeaveCriticalSection(CS_UserIPArr);
   end
else if Uppercase(ReportType) = 'SHAREIP' then
   begin
   ReportTitle := 'Shares IP Report';
   Dataname    := 'IP';
   SetLength(CopyArray,0);
   EnterCriticalSection(CS_ShareIPArr);
   CopyArray := Copy(ShareIPArr,0,length(ShareIPArr));
   LeaveCriticalSection(CS_ShareIPArr);
   end
else if Uppercase(ReportType) = 'WRONGMINER' then
   begin
   ReportTitle := 'Wrong Shares Miner Report';
   Dataname    := 'MinerApp';
   SetLength(CopyArray,0);
   EnterCriticalSection(CS_WrongShareMiner);
   CopyArray := Copy(WrongShareMiner,0,length(WrongShareMiner));
   LeaveCriticalSection(CS_WrongShareMiner);
   end
else if Uppercase(ReportType) = 'WRONGIP' then
   begin
   ReportTitle := 'Wrong Shares IP Report';
   Dataname    := 'User IP';
   SetLength(CopyArray,0);
   EnterCriticalSection(CS_WrongShareIp);
   CopyArray := Copy(WrongShareIp,0,length(WrongShareIp));
   LeaveCriticalSection(CS_WrongShareIp);
   end
else
   begin
   ToLog('.Unknown report type: '+ReportType,uToConsole);
   exit;
   end;
SetLength(FinalArray,0);
for counter := 0 to length(CopyArray)-1 do
   begin
   Added := false;
   for count2 := 0 to length(FinalArray)-1 do
      begin
      if CopyArray[counter].counter>FinalArray[count2].counter then
         begin
         Insert(CopyArray[counter],FinalArray,count2);
         Added := true;
         break;
         end;
      end;
   if not Added then Insert(CopyArray[counter],FinalArray,Length(FinalArray));
   end;

if Destination = uToConsole then
   begin
   if ( (NumberRecords=0) or (numberRecords>length(FinalArray)) ) then
      NumberRecords := length(FinalArray);
   ToLog(','+ReportTitle,uToConsole);
   ToLog('/--------------------------------------',uToConsole);
   MinerApp := Format('%0:-20s',[Dataname]);
   thisCounter := Format('%0:-5s',['Total']);
   thisinBlock := Format('%0:-5s',['Block']);
   ToLog(Format('/%s | %s | %s |',[MinerApp,ThisCounter, ThisinBlock]),uToConsole);
   ToLog('/--------------------------------------',uToConsole);
   for counter := 0 to NumberRecords-1 do
      begin
      MinerApp := Format('%0:-20s',[FinalArray[counter].data]);
      thisCounter := Format('%0:5s',[FinalArray[counter].counter.ToString]);
      thisinBlock := Format('%0:5s',[FinalArray[counter].inBlock.ToString]);
      ToLog(Format(' %s | %s | %s |',[MinerApp,ThisCounter,ThisInBlock]),uToConsole);
      Inc(TotalRecords);
      end;
   if TotalRecords= 0 then ToLog('.No records found',uToConsole);
   ToLog('/--------------------------------------',uToConsole);
   end;

End;

Procedure BuildNewBlock();
var
  BlockFile  : TextFile;
  Number     : integer;
  Distribute : string;
  BlockSpeed, TotalShares : int64;
Begin
Number := GetMainConsensus.block;
TotalShares := SharesCount;
AssignFile(BlockFile,'blocks'+DirectorySeparator+Number.ToString+'.txt');
TRY
   Rewrite(BlockFile);
   Writeln(BlockFile,Format('Block    : %s',[number.ToString]));
   WriteLn(Blockfile,format('Miner    : %s',[GetMainConsensus.LBMiner]));
   WriteLn(Blockfile,format('SolDiff  : %s',[GetMainConsensus.LBSolDiff]));
   WriteLn(Blockfile,format('PoW      : %s',[Int2Curr(GetMainConsensus.LBPoW)]));
   WriteLn(Blockfile,format('Miners   : %d',[MinersCount]));
   WriteLn(Blockfile,format('Shares   : %d',[TotalShares]));
   BlockSpeed := (SharesCount*SESSION_HashPerShare) div 575;
   SetLastBlockRate(BlockSpeed);
   WriteLn(Blockfile,format('Speed    : %d h/s',[BlockSpeed]));
   WriteLn(Blockfile,format('Best     : %s',[GetBlockBest]));
   if GetMainConsensus.LBMiner = PoolAddress then
      begin
      Distribute := DistributeBlockPayment();
      WriteLn(Blockfile,format('PerShare : %s',[Int2Curr(StrToIntDef(Parameter(Distribute,0),0))]));
      WriteLn(Blockfile,format('Earned   : %s',[Int2Curr(StrToIntDef(Parameter(Distribute,1),0))]));
      ToLog('.Block mined: '+number.ToString);
      end;
   CloseFile(BlockFile);
   ToLog(Format(' Created block : %s [Debt: %s]',[number.ToString,Int2Curr(GetTotalDebt)]))
EXCEPT ON E:Exception do
END; {TRY}
SaveMiners();
UpdatePoolBalance;
Insert(GetDiffHashrate(GetMainConsensus.LBSolDiff),ArraySols,length(ArraySols));
Delete(ArraySols,0,1);
CalculateMainNetHashrate;
Insert(GetMainConsensus.LBMiner,ArrayMiner,length(ArrayMiner));
ThisBlockMNs := false;
Delete(ArrayMiner,0,1);
GetBlocksMinedByPool;
if AutoDiff then
   begin
   if TotalShares<AutoValue then
      begin
      Setlength(MinDiffBase,length(MinDiffBase)-1);
      MinerDiff := AddCharR('F',MinDiffBase,32);
      SaveConfig;
      RefreshPoolHeader := true;
      SESSION_HashPerShare := Round(Power(16,GetDiffHashrate(MinerDiff)/100));
      end;
   if TotalShares>AutoValue*16 then
      begin
      MinDiffBase := MinDiffBase+'0';
      MinerDiff := AddCharR('F',MinDiffBase,32);
      SaveConfig;
      RefreshPoolHeader := true;
      SESSION_HashPerShare := Round(Power(16,GetDiffHashrate(MinerDiff)/100));
      end;
   end;
End;

Procedure ResetBlock();
Begin
MainBestDiff := DefWorst;
if GetMainConsensus.block > GetMyLastUpdatedBlock then
   begin
   BuildNewBlock();
   end;
EnterCriticalSection(CS_Shares);
SetLength(ArrShares,0);
LeaveCriticalSection(CS_Shares);
ResetPrefixIndex();
SetBlockBest(DefWorst);
SetSolution(Default(TSolution));
SESSION_Shares := 0;
SESSION_Started := UTCTime;
RejectedShares := 0;
if StoreShares then SaveShareIndex;
ClearUserMinerArray(False);
ClearUserIPArray(False);
ClearShareIPArray(False);
ClearWrongShareMiner(False);
ClearWrongShareIP(False);
End;

Function GetPrefixIndex():Integer;
Begin
EnterCriticalSection(CS_PrefixIndex);
Inc(BlockPrefixesRequested);
Result := PrefixIndex;
Inc(PrefixIndex);
LeaveCriticalSection(CS_PrefixIndex);
End;

Procedure ResetPrefixIndex();
Begin
EnterCriticalSection(CS_PrefixIndex);
PrefixIndex := 0;
BlockPrefixesRequested := 0;
LeaveCriticalSection(CS_PrefixIndex);
End;

function GetPrefixStr(IndexValue:integer = -1):string;
var
  Index, firstchar, secondchar, ThirdChar : integer;
  HashChars : integer;
Begin
if IndexValue = -1 then Index := GetPrefixIndex()
else Index := IndexValue;
HashChars := length(HasheableChars)-1;
firstchar := Index div (HashChars*HashChars);
Index     := Index mod (HashChars*HashChars);
secondchar:= Index div HashChars;
ThirdChar := Index mod HashChars;
result := HasheableChars[firstchar+1]+HasheableChars[secondchar+1]+HasheableChars[ThirdChar+1];
End;

function CheckIPMiners(UserIP:String):Boolean;
Begin
result := true;
End;

// Try to send a message safely
Function TryMessageToMiner(AContext: TIdContext;message:string):boolean;
Begin
result := true;
TRY
Acontext.Connection.IOHandler.WriteLn(message);
EXCEPT on E:Exception do
   begin
   ToLog(' Error sending message to miner: '+E.Message,uToFile);
   end;
END;{Try}
End;

// Try to close a pool connection safely
Procedure TryClosePoolConnection(AContext: TIdContext; closemsg:string='');
Begin
if closemsg <>'' then
   begin
   TryMessageToMiner(AContext,closemsg);
   end;
TRY
Acontext.Connection.IOHandler.InputBuffer.Clear;
AContext.Connection.Disconnect;
AContext.Connection.IOHandler.DiscardAll(); // ?? is valid
EXCEPT on E:Exception do
   ToLog(' Error closing miner conneciton: '+E.Message,uToFile);
END;{Try}
End;

Class Procedure PoolServerEvents.OnExecute(AContext: TIdContext);
Begin
// Probably unnecessary
End;

Class Procedure PoolServerEvents.OnConnect(AContext: TIdContext);
var
  IPUser, Linea    : String;
  Command, Address, ThisShare : String;
  ValidShareValue : integer;
  MinerData       : string;
    MinBal : int64;
    MinTill: integer;
    MinPay : string;
  MinerDevice     : string = '';
Begin
//*******************
// IMPLEMENT TIME FILTER WITH BLOCKAGE
IPUser := AContext.Connection.Socket.Binding.PeerIP;
Linea := '';
TRY
Linea := AContext.Connection.IOHandler.ReadLn('',3000,-1,IndyTextEncoding_UTF8);
EXCEPT On E:Exception do
   begin
   TRY
   AContext.Connection.Disconnect;
   EXCEPT On E:Exception do
      begin
      TryClosePoolConnection(AContext);
      exit;
      end
   END{Try};
   end;
END{Try};
Command := Parameter(Linea,0);
Address := Parameter(Linea,1);
If UpperCase(Command) = 'SOURCE' then
   begin
   AddUserMiner(Parameter(Linea,2));
   AddUserIP(IPUser);
   if ( (not IsValidHashAddress(Address)) or (address = '') ) then
      begin
      TryClosePoolConnection(AContext,'WRONG_ADDRESS');
      exit;
      end;
   if CheckIPMiners(IPUser) then
      begin
      MinerData := GetMinerData(Address);
      MinBal := StrToInt64Def(Parameter(MinerData,0),0);
      MinTill:= StrToIntDef(Parameter(MinerData,1),0);
      MinPay := Parameter(MinerData,2);
      // 1{MinerPrefix} 2{MinerAddress} 3{PoolMinDiff} 4{LBHash} 5{LBNumber} 6{MinerBalance}
      // 7{TillPayment} 8{LastPayInfo} 9{LastBlockPoolHashrate} {10}MainnetHashRate {11}PoolFee

      TryClosePoolConnection(AContext,'OK '+{1}GetPrefixStr+' '+
                                            {2}PoolAddress+' '+
                                            {3}MinerDiff+' '+
                                            {4}GetMainConsensus.LBHash+' '+
                                            {5}GetMainConsensus.block.ToString+' '+
                                            {6}MinBal.ToString+' '+
                                            {7}MinTill.ToString+' '+
                                            {8}MinPay+' '+
                                            {9}GetLastBlockRate.ToString+' '+
                                            {10}MainnetHashRate.ToString+' '+
                                            {11}PoolFee.ToString);
      end;
   end
else If UpperCase(Command) = 'SHARE' then
   //{1}Addess {2}Share {3}Miner
   begin
   ThisShare   := Parameter(Linea,2);
   ValidShareValue := ShareIsValid(ThisShare,Address,MinerDevice, IPUser);
   if ValidShareValue=0 then
      begin
      TryClosePoolConnection(AContext,'True');

      end
   else
      begin
      TryClosePoolConnection(AContext,'False '+ValidShareValue.ToString);

      end
   end
{
else If UpperCase(Command) = 'POOLSTATUS' then
   begin

   end
}
else If UpperCase(Command) = 'POOLINFO' then
   begin
   TryClosePoolConnection(AContext,minerscount.ToString+' '+GetLastBlockRate.ToString+' '+PoolFee.ToString);
   end
else
   begin
   TryClosePoolConnection(AContext,'Unknown :'+Linea);
   end;
End;

Procedure SendSolution(Data:TSolution);
var
  TCPClient        : TidTCPClient;
  Node             : integer;
  Resultado        : string;
  Trys             : integer = 0;
  Success, WasGood : boolean;
  Mainnetbest      : string;
  ErrorCode        : String;
Begin
Node := Random(LengthNodes);
TCPClient := TidTCPClient.Create(nil);
TCPclient.ConnectTimeout:= 3000;
TCPclient.ReadTimeout:=3000;
REPEAT
Node := Node+1; If Node >= LengthNodes then Node := 0;
TCPclient.Host:=GetNodeIndex(Node).host;
TCPclient.Port:=GetNodeIndex(Node).port;
Success := false;
Trys :=+1;
TRY
TCPclient.Connect;
TCPclient.IOHandler.WriteLn('BESTHASH 1 2 3 4 '+PoolAddress+' '+GetSolution.Hash+' '+IntToStr(GetMainConsensus.block+1)+' '+UTCTime.ToString);


Resultado := TCPclient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
TCPclient.Disconnect();
Success := true;
EXCEPT on E:Exception do
   begin
   Success := false;
   end;
END{try};
UNTIL ((Success) or (Trys = 5));
TCPClient.Free;
If success then
   begin
   WasGood := StrToBoolDef(Parameter(Resultado,0),false);
   ErrorCode := Parameter(Resultado,2);
   Mainnetbest := Parameter(Resultado,1);
   MainBestDiff := Mainnetbest;

   if WasGood then
      begin
      ToLog(',Besthash submited: '+BestHashReadeable(Data.Diff),uToFile);
      Inc(SESSION_BestHashes);
      end;
   end
else
   begin
   ToLog(' Unable to send solution',uToFile);
   SetSolution(GetSolution);
   end;
End;

Procedure SetPayThreads(Tvalue:integer);
Begin
EnterCriticalSection(CS_PayThreads);
OpenPayThreads := TValue;
GoodPayments := 0;
BadPayments  := 0;
LeaveCriticalSection(CS_PayThreads);
End;

Function GetPayThreads():integer;
Begin
EnterCriticalSection(CS_PayThreads);
Result := OpenPayThreads;
LeaveCriticalSection(CS_PayThreads);
End;

Procedure DecreasePayThreads(WasGood:boolean);
Begin
EnterCriticalSection(CS_PayThreads);
OpenPayThreads := OpenPayThreads-1;
if wasGood then Inc(GoodPayments)
else Inc(BadPayments);
LeaveCriticalSection(CS_PayThreads);
End;

Procedure SetPoolBalance(ThisValue:int64);
Begin
EnterCriticalSection(CS_PoolBalance);
PoolBalance := ThisValue;
LeaveCriticalSection(CS_PoolBalance);
End;

Function GetPoolBalance():Int64;
Begin
EnterCriticalSection(CS_PoolBalance);
Result := PoolBalance;
LeaveCriticalSection(CS_PoolBalance);
End;

Procedure UpdatePoolBalance();
var
  ThisThread : ThreadUpdateBalance;
Begin
ThisThread := ThreadUpdateBalance.Create(true);
ThisThread.FreeOnTerminate:=true;
ThisThread.Start;
End;

Procedure SetLastBlockRate(ThisValue:int64);
Begin
EnterCriticalSection(CS_LastBlockRate);
LastBlockRate := ThisValue;
LeaveCriticalSection(CS_LastBlockRate);
End;

Function GetLastBlockRate():Int64;
Begin
EnterCriticalSection(CS_LastBlockRate);
Result := LastBlockRate;
LeaveCriticalSection(CS_LastBlockRate);
End;

Procedure SaveShareIndex();
var
  ThisFile : File of integer;
  counter : integer;
Begin
Assignfile(ThisFile, 'frequency.dat');
TRY
Rewrite(ThisFile);
for counter := 0 to 15 do
   begin
   Seek(ThisFile,counter);
   Write(ThisFile,ShareIndex[counter]);
   end;
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION DO
   ToLog(' Error saving share index: '+E.Message);
END;{TRY}
if StoreShares then ShareIndexReport;
End;

Procedure LoadShareIndex();
var
  ThisFile : File of integer;
  counter : integer;
Begin
Assignfile(ThisFile, 'frequency.dat');
TRY
reset(ThisFile);
for counter := 0 to 15 do
   begin
   Seek(ThisFile,counter);
   Read(ThisFile,ShareIndex[counter]);
   end;
CloseFile(ThisFile);
EXCEPT ON E:EXCEPTION DO
   ToLog(' Error loading share index: '+E.Message);
END;{TRY}
End;

Procedure CreditFrequency(Diff : string);
var
  DiffLen  : integer;
  ThisChar : String;
  ThisValue : integer;
Begin
TRY
DiffLen := Length(MinDiffBase)+1;
ThisChar := Diff[Difflen];
ThisValue := Hex2Dec(ThisChar);
Inc(ShareIndex[ThisValue]);
EXCEPT ON E:EXCEPTION DO
   ToLog(' Error crediting frequency: '+E.Message);
END;{TRY}
End;

Procedure ShareIndexReport();
var
  counter  : integer;
  ThisFile : textfile;
  text1, text2, text3 : string;
  TotalShares : integer = 0;
Begin
TRY
Assignfile(ThisFile, 'sharereport.txt');
for counter := 1 to 15 do
   TotalShares := TotalShares+shareindex[counter];
rewrite(ThisFile);
writeln(ThisFile,format('TotalShares: %d',[TotalShares]));
for counter := 1 to 15 do
   begin
   Text1 := Uppercase(IntToHex(counter,1));
   Text2 := Format('%0:5d',[shareindex[counter]]);
   Text3 := formatFloat('0.00',(shareindex[counter]*100)/totalshares)+'%';
   Text3 := Format('%0:6s',[Text3]);
   writeln(ThisFile,format('%s %s %s',[text1,text2,text3]));
   end;
CloseFile(ThisFile);
ToLog(' Share index report created');
EXCEPT ON E:EXCEPTION do
   ToLog(' Error saving sharereport.txt: '+E.message);
END;{TRY}
End;

Function GetBlockValue(Block:integer;valuename:string):string;
var
  ThisFile : Textfile;
  FileName : string;
  Texto    : string;
Begin
Result := '';
FileName := 'blocks'+DirectorySeparator+Block.ToString+'.txt';
If fileExists(FileName) then
   begin
   AssignFile(ThisFile,Filename);
   TRY
   Reset(ThisFile);
   while not Eof(thisfile) do
      begin
      Readln(ThisFile,Texto);
      Texto := DelSpace1(Texto);
      if Uppercase(Parameter(texto,0)) = UpperCase(Valuename) then
         begin
         result := Parameter(texto,2);
         break;
         end;
      end;
   CloseFile(ThisFile);
   EXCEPT ON E:EXCEPTION do
      ToLog(' Error getting block value: '+E.Message);
   END;{TRY}
   end
else
   begin

   end;
End;

Procedure FillSolsArray();
var
  counter        : integer;
  ThisBlockDiff  : string;
  Thisblockminer : string;
  ThisBlockValue : integer;
  LastBlock  : integer;
Begin
SetLength(ArraySols,0);
SetLEngth(ArrayMiner,0);
LastBlock := GetMyLastUpdatedBlock;
for counter := LastBlock-99 to LastBlock do
   begin
   ThisBlockDiff := GetBlockValue(counter,'SolDiff');
   if thisblockDiff = '' then thisblockDiff := DefWorst;
   ThisBlockValue := GetDiffHashrate(thisblockDiff);
   Insert(ThisBlockValue,ArraySols,length(ArraySols));
   Thisblockminer := GetBlockValue(counter,'Miner');
   Insert(Thisblockminer,ArrayMiner,length(ArrayMiner));
   end;
CalculateMainNetHashrate;
GetBlocksMinedByPool();
End;

Procedure CalculateMainNetHashrate();
var
  counter        : integer;
  TotalValue     : integer = 0;
  TotalRate : double = 0;
Begin
for counter := 0 to length(ArraySols)-1 do
   TotalValue := TotalValue+ArraySols[counter];

TotalRate := (TotalValue/100)/length(ArraySols);

TotalRate := Power(16,TotalRate);
TotalRate := TotalRate/(575);
MainnetHashRate := Round(TotalRate);
End;

Procedure GetBlocksMinedByPool();
var
  mined : integer = 0;
  counter : integer;
Begin
for counter := 0 to length(ArrayMiner)-1 do
   if ArrayMiner[counter] = PoolAddress then Inc(Mined);
BlocksMinedByPool := mined;
End;

// ********
// Counters
// ********

Procedure AddUserMiner(Minerv:String);
var
  counter : integer;
  Added : boolean = false;
Begin
if minerv = '' then Minerv := 'Unknown';
EnterCriticalSection(CS_USerMiner);
For counter := 0 to length(UserMiner)-1 do
   begin
   if userminer[counter].data=Minerv then
      begin
      added := true;
      Inc(UserMiner[counter].counter);
      Inc(UserMiner[counter].inblock);
      break;
      end;
   end;
if not added then
   begin
   SetLEngth(Userminer,LEngth(UserMiner)+1);
   Userminer[length(userminer)-1].data:=minerv;
   Userminer[length(userminer)-1].counter:=1;
   Userminer[length(userminer)-1].inBlock:=1;
   end;
LEaveCriticalSection(CS_USerMiner);
End;

Procedure ClearUserMinerArray(ClearAll:boolean = true);
var
  counter:integer;
Begin
if ClearAll then
   begin
   EnterCriticalSection(CS_USerMiner);
   SetLEngth(UserMiner,0);
   LeaveCriticalSection(CS_USerMiner);
   end
else
   begin
   EnterCriticalSection(CS_USerMiner);
   For counter := 0 to length(USerMiner)-1 do
      USerMiner[counter].inblock:=0;
   LeaveCriticalSection(CS_USerMiner);
   end;
End;

Procedure AddUserIP(userip:string);
var
  counter : integer;
  Added : boolean = false;
Begin
if userip = '' then userip := 'Unknown';
EnterCriticalSection(CS_UserIPArr);
For counter := 0 to length(UserIPArr)-1 do
   begin
   if UserIPArr[counter].data=UserIP then
      begin
      added := true;
      Inc(UserIPArr[counter].counter);
      Inc(UserIPArr[counter].inblock);
      break;
      end;
   end;
if not added then
   begin
   SetLEngth(UserIPArr,LEngth(UserIPArr)+1);
   UserIPArr[length(UserIPArr)-1].data:=UserIP;
   UserIPArr[length(UserIPArr)-1].counter:=1;
   UserIPArr[length(UserIPArr)-1].inBlock:=1;
   end;
LEaveCriticalSection(CS_UserIPArr);
End;

Procedure ClearUserIPArray(ClearAll:boolean = true);
var
  counter:integer;
Begin
if ClearAll then
   begin
   EnterCriticalSection(CS_UserIPArr);
   SetLEngth(UserIPArr,0);
   LeaveCriticalSection(CS_UserIPArr);
   end
else
   begin
   EnterCriticalSection(CS_UserIPArr);
   For counter := 0 to length(UserIPArr)-1 do
      UserIPArr[counter].inblock:=0;
   LeaveCriticalSection(CS_UserIPArr);
   end;
End;

Procedure AddShareIP(userip:string);
var
  counter : integer;
  Added : boolean = false;
Begin
if userip = '' then userip := 'Unknown';
EnterCriticalSection(CS_ShareIPArr);
For counter := 0 to length(ShareIPArr)-1 do
   begin
   if ShareIPArr[counter].data=UserIP then
      begin
      added := true;
      Inc(ShareIPArr[counter].counter);
      Inc(ShareIPArr[counter].inblock);
      break;
      end;
   end;
if not added then
   begin
   SetLEngth(ShareIPArr,LEngth(ShareIPArr)+1);
   ShareIPArr[length(ShareIPArr)-1].data:=UserIP;
   ShareIPArr[length(ShareIPArr)-1].counter:=1;
   ShareIPArr[length(ShareIPArr)-1].inBlock:=1;
   end;
LeaveCriticalSection(CS_ShareIPArr);
End;

Procedure ClearShareIPArray(ClearAll:boolean = true);
var
  counter:integer;
Begin
if ClearAll then
   begin
   EnterCriticalSection(CS_ShareIPArr);
   SetLEngth(ShareIPArr,0);
   LeaveCriticalSection(CS_ShareIPArr);
   end
else
   begin
   EnterCriticalSection(CS_ShareIPArr);
   For counter := 0 to length(ShareIPArr)-1 do
      ShareIPArr[counter].inblock:=0;
   LeaveCriticalSection(CS_ShareIPArr);
   end;
End;

Procedure AddWrongShareMiner(userip:string);
var
  counter : integer;
  Added : boolean = false;
Begin
if userip = '' then userip := 'Unknown';
EnterCriticalSection(CS_WrongShareMiner);
For counter := 0 to length(WrongShareMiner)-1 do
   begin
   if WrongShareMiner[counter].data=UserIP then
      begin
      added := true;
      Inc(WrongShareMiner[counter].counter);
      Inc(WrongShareMiner[counter].inblock);
      break;
      end;
   end;
if not added then
   begin
   SetLEngth(WrongShareMiner,LEngth(WrongShareMiner)+1);
   WrongShareMiner[length(WrongShareMiner)-1].data:=UserIP;
   WrongShareMiner[length(WrongShareMiner)-1].counter:=1;
   WrongShareMiner[length(WrongShareMiner)-1].inBlock:=1;
   end;
LeaveCriticalSection(CS_WrongShareMiner);
End;

Procedure ClearWrongShareMiner(ClearAll:boolean = true);
var
  counter:integer;
Begin
if ClearAll then
   begin
   EnterCriticalSection(CS_WrongShareMiner);
   SetLEngth(WrongShareMiner,0);
   LeaveCriticalSection(CS_WrongShareMiner);
   end
else
   begin
   EnterCriticalSection(CS_WrongShareMiner);
   For counter := 0 to length(WrongShareMiner)-1 do
      WrongShareMiner[counter].inblock:=0;
   LeaveCriticalSection(CS_WrongShareMiner);
   end;
End;

Procedure AddWrongShareIp(ThisData:string);
var
  counter : integer;
  Added : boolean = false;
Begin
if ThisData = '' then ThisData := 'Unknown';
EnterCriticalSection(CS_WrongShareIp);
For counter := 0 to length(WrongShareIp)-1 do
   begin
   if WrongShareIp[counter].data=ThisData then
      begin
      added := true;
      Inc(WrongShareIp[counter].counter);
      Inc(WrongShareIp[counter].inblock);
      break;
      end;
   end;
if not added then
   begin
   SetLEngth(WrongShareIp,LEngth(WrongShareIp)+1);
   WrongShareIp[length(WrongShareIp)-1].data:=ThisData;
   WrongShareIp[length(WrongShareIp)-1].counter:=1;
   WrongShareIp[length(WrongShareIp)-1].inBlock:=1;
   end;
LeaveCriticalSection(CS_WrongShareIp);
End;

Procedure ClearWrongShareIp(ClearAll:boolean = true);
var
  counter:integer;
Begin
if ClearAll then
   begin
   EnterCriticalSection(CS_WrongShareIp);
   SetLEngth(WrongShareIp,0);
   LeaveCriticalSection(CS_WrongShareIp);
   end
else
   begin
   EnterCriticalSection(CS_WrongShareIp);
   For counter := 0 to length(WrongShareIp)-1 do
      WrongShareIp[counter].inblock:=0;
   LeaveCriticalSection(CS_WrongShareIp);
   end;
End;




END. // End unit

