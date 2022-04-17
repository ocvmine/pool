UNIT consopooldata;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, coreunit, IdTCPServer, IdContext, IdGlobal, StrUtils,
  IdTCPClient, math, fileutil;

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

Procedure AddPaymentToFile(Block,destino,monto,OrderID:String);
function SendOrder(OrderString:String):String;
function GetAddressBalance(Address:String):int64;

Procedure CreateMinersFile();
Procedure SaveMiners();
Procedure CreateBlockzero();
Procedure createPaymentsFile();
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
Procedure CreditShare(Address:String);
Function ShareIsValid(Share,Address:String):integer;

Procedure SetSolution(Data:TSolution);
Function GetSolution():TSolution;

Function ResetLogs():boolean;
Procedure RawToLog(Linea:String);
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
Procedure ToLog(Texto:string);
Function GetBlockBest():String;
Procedure SetBlockBest(ThisValue:String);
Function DistributeBlockPayment():string;
Procedure RunPayments();
Procedure GenerateReport();
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


CONST
  fpcVersion = {$I %FPCVERSION%};
  AppVersion = 'v0.31';
  DefHelpLine= 'Type help for available commands';
  DefWorst = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';

VAR
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
  // Operative
  ArraySols    : array of integer;
  ShareIndex   : array[0..15] of integer;
  Command      : string  = '';
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
  // Mainnet
  LastConsensusTry: int64   = 0;
  WaitingConsensus:Boolean = false;
  CurrentBlock    : integer = 0;
  MainBestDiff    : String = DefWorst;
  MainnetHashRate : int64 = 0;
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
  CS_UpdateScreen : TRTLCriticalSection;
  CS_PrefixIndex  : TRTLCriticalSection;
  CS_LogLines     : TRTLCriticalSection;
  CS_Miners       : TRTLCriticalSection;
  CS_Shares       : TRTLCriticalSection;
  CS_BlockBest    : TRTLCriticalSection;
  CS_Solution     : TRTLCriticalSection;
  CS_PaysFile     : TRTLCriticalSection;
  CS_PayThreads   : TRTLCriticalSection;
  CS_PoolBalance  : TRTLCriticalSection;
  CS_LastBlockRate: TRTLCriticalSection;

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
TrxTime := UTCTime;
trfHash := GetTransferHash(TrxTime.ToString+PoolAddress+address+ToSend.ToString+IntToStr(GetMainConsensus.block));
OrdHash := GetOrderHash(TrxTime.ToString+'1'+trfHash);

SignString := GetStringSigned(TrxTime.ToString+PoolAddress+Address+ToSend.ToString+
                     Fee.ToString+'1',PrivateKey);

TextToSend := 'NSLORDER 1 0.16 '+TrxTime.ToString+' ORDER 1 $TRFR '+OrdHash+' 1 TRFR '+TrxTime.ToString+
              ' Pool_Payment 1 '+PublicKey+' '+PoolAddress+' '+Address+' '+Fee.ToString+' '+ToSend.ToString+' '+
              SignString+' '+trfHash;
Resultado  := SendOrder(TextToSend);
if Resultado <> '' then
   begin
   //ToLog(OrdHash+'->'+Resultado);
   LastPayInfo := (GetMainConsensus.block+1).ToString+':'+ToSend.ToString+':'+Resultado;
   ClearAddressBalance(Address, LastPayInfo);
   AddPaymentToFile((GetMainConsensus.block+1).ToString,Address,Balance.ToString,Resultado);
   WasGood := true;
   end;
DecreasePayThreads(WasGood);
End;

Procedure AddPaymentToFile(Block,destino,monto,OrderID:String);
Begin
EnterCriticalSection(CS_PaysFile);
Append(PaysFile);
Writeln(PaysFile,Format('%s %s %s %s',[block,destino,monto,OrderID]));
CloseFile(PaysFile);
LeaveCriticalSection(CS_PaysFile);
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
   //Tolog(OrderString);
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
      ToLog('Error getting address balance: '+E.Message);
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
//Rewrite(TempMinersFile);
for counter := 0 to length(ArrMiners)-1 do
   begin
   if ( (ArrMiners[counter].Shares>0) or (ArrMiners[counter].Balance>0) ) then
      begin
      if not AnsiContainsStr(AlreadyAdded,ArrMiners[counter].address) then
         begin
         write(MinersFile,ArrMiners[counter]);
         AlreadyAdded := AlreadyAdded+ArrMiners[counter].address+',';
         end;
      {
      ThisTemp.address:=ArrMiners[counter].address;
      ThisTemp.Shares:=ArrMiners[counter].Shares;
      ThisTemp.Balance:=ArrMiners[counter].Balance;
      ThisTemp.LastPay:=ArrMiners[counter].LastPay;
      ThisTemp.LastPayOrder:='';
      write(TempMinersFile,ThisTemp);
      }
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
   writeln('Error creating block 0');
   Halt(1);
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
      tolog ('Error getting my last updated block');
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

Procedure CreditShare(Address:String);
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

Function ShareIsValid(Share,Address:String):integer;
var
  ThisHash, ThisDiff : string;
  ThisSolution : TSolution;
Begin
Result := 0;
if ShareAlreadyExists(Share) then
   begin
   ToLog('Duplicated: '+share);
   result := 4;
   end
else if ((length(Share)<18) or (length(Share)>33)) then
   begin
   ToLog('WrongLength: '+share);
   result := 7;
   end
else
   begin
   ThisHash := NosoHash(Share+PoolAddress);
   ThisDiff := CheckHashDiff(GetMainConsensus.LBHash,ThisHash);
   if ThisDiff < MinerDiff then // Valid share
      begin
      Result := 0;
      AddShare(Share+Address);
      CreditShare(Address);
      CreditFrequency(ThisDiff);
      Inc(SESSION_Shares);
      UpdateServerInfo := true;
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
      //ToLog('Invalid share: '+ThisDiff);
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
If not FileExists('oldlogs.txt') then
   Begin
   Rewrite(OldLogFile);
   CloseFile(OldLogFile);
   end;
If not FileExists('log.txt') then
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
   Result := false;
END {Try};
End;

Procedure RawToLog(Linea:String);
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
ToLog('Server started at port '+PoolPort.ToString);
SESSION_Started := UTCTime;
SESSION_HashPerShare := Round(Power(16,GetDiffHashrate(MinerDiff)/100));
ToLog('Hashes Per Share : '+SESSION_HashPerShare.ToString);
if Offset <> 0 then ToLog('Time offset : '+Offset.ToString);
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

Procedure ToLog(Texto:string);
Begin
if Texto[1] = ',' then
   begin
   Texto := copy(Texto,2, length(texto));
   Texto := ','+DateTimeToStr(now)+' '+Texto;
   end
else if Texto[1] = '.' then
   begin
   Texto := copy(Texto,2, length(texto));
   Texto := '.'+DateTimeToStr(now)+' '+Texto;
   end
else if Texto[1] = '/' then
   begin
   Texto := copy(Texto,2, length(texto));
   Texto := '/'+DateTimeToStr(now)+' '+Texto;
   end
else Texto := DateTimeToStr(now)+' '+Texto;
EnterCriticalSection(CS_LogLines);
Insert(Texto,LogLines,Length(LogLines));
LeaveCriticalSection(CS_LogLines);
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
   ToLog(Format('Paying to %d addresses : %s',[PayingAddresses,Int2Curr(TotalToPay)]));
   SetPayThreads(PayingAddresses);
   CheckPaysThreads := true;
   end
else
   begin
   SaveMiners();
   GenerateReport();
   UpdatePoolBalance;
   end;
End;

Procedure GenerateReport();
var
  CopyArray, finalArray : Array of TMinersData ;
  counter, counter2   : integer;
  Inserted : boolean;
  ReportFile : TextFile;
  BalanceTotal : int64 = 0;
  AddText,BalanText : string;
Begin
SetLength(CopyArray,0);
SetLength(finalArray,0);
EnterCriticalSection(CS_Miners);
CopyArray := copy(ArrMiners,0,length(ArrMiners));
LeaveCriticalSection(CS_Miners);
for counter := 0 to length(CopyArray)-1 do
   begin
   if CopyArray[counter].Balance>0 then
      begin
      BalanceTotal := BalanceTotal+ CopyArray[counter].Balance;
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
AssignFile(ReportFile,'report.txt');
Rewrite(ReportFile);
writeln(ReportFile,'Block : '+GetMAinConsensus.block.ToString());
writeln(ReportFile,'Miners: '+Length(FinalArray).ToString());
WriteLn(ReportFile,'Debt  : '+Int2Curr(BalanceTotal));
Writeln(ReportFile,'');
for counter2 := 0 to length(finalArray)-1 do
   begin
   AddText   := format('%0:-35s',[FinalArray[counter2].address]);
   BalanText := Format('%0:12s',[Int2Curr(FinalArray[counter2].balance)]);
   writeln(ReportFile,format('%35s %s',[AddText,BalanText]));
   end;
CloseFile(ReportFile);
ToLog(format('/Report file Generated [ %s ]',[Int2Curr(GetAddressBalance(PoolAddress)-BalanceTotal)]));
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
   ToLog(Format('Created block : %s [Debt: %s]',[number.ToString,Int2Curr(GetTotalDebt)]))
EXCEPT ON E:Exception do
END; {TRY}
SaveMiners();
UpdatePoolBalance;
Insert(GetDiffHashrate(GetMainConsensus.LBSolDiff),ArraySols,length(ArraySols));
Delete(ArraySols,0,1);
CalculateMainNetHashrate;
if AutoDiff then
   begin
   if TotalShares<100 then
      begin
      Setlength(MinDiffBase,length(MinDiffBase)-1);
      MinerDiff := AddCharR('F',MinDiffBase,32);
      SaveConfig;
      RefreshPoolHeader := true;
      SESSION_HashPerShare := Round(Power(16,GetDiffHashrate(MinerDiff)/100));
      end;
   if TotalShares>1600 then
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
SaveShareIndex;
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
   ToLog('Error sending message to miner: '+E.Message);
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
   ToLog('Error closing miner conneciton: '+E.Message);
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
Begin
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
//ToLog('Line received from '+IPUser+'->'+Linea);
If UpperCase(Command) = 'SOURCE' then
   begin
   if CheckIPMiners(IPUser) then
      begin
      MinerData := GetMinerData(Address);
      MinBal := StrToInt64Def(Parameter(MinerData,0),0);
      MinTill:= StrToIntDef(Parameter(MinerData,1),0);
      MinPay := Parameter(MinerData,2);
      // 1{MinerPrefix} 2{MinerAddress} 3{PoolMinDiff} 4{LBHash} 5{LBNumber} 6{MinerBalance}
      // 7{TillPayment} 8{LastPayInfo} 9{LastBlockPoolHashrate}
      //ToLog('Miner from '+IPUser);
      TryClosePoolConnection(AContext,'OK '+{1}GetPrefixStr+' '+
                                            {2}PoolAddress+' '+
                                            {3}MinerDiff+' '+
                                            {4}GetMainConsensus.LBHash+' '+
                                            {5}GetMainConsensus.block.ToString+' '+
                                            {6}MinBal.ToString+' '+
                                            {7}MinTill.ToString+' '+
                                            {8}MinPay+' '+
                                            {9}GetLastBlockRate.ToString+' '+
                                            {10}MainnetHashRate.ToString);
      end;
   end
else If UpperCase(Command) = 'SHARE' then
   begin
   ThisShare := Parameter(Linea,2);
   ValidShareValue := ShareIsValid(ThisShare,Address);
   if ValidShareValue=0 then
      begin
      TryClosePoolConnection(AContext,'True');
      //ToLog('Valid share from '+Address);
      end
   else
      begin
      TryClosePoolConnection(AContext,'False '+ValidShareValue.ToString);
      //ToLog(Format('Wrong Share: %s (%s)',[ValidShareValue.ToString,IPUser]));
      end
   end
{
else If UpperCase(Command) = 'POOLSTATUS' then
   begin

   end
}
else
   begin
   TryClosePoolConnection(AContext);
   end;
End;

Procedure SendSolution(Data:TSolution);
var
  TCPClient : TidTCPClient;
  Node : integer;
  Resultado : string;
  Trys : integer = 0;
  Success, WasGood : boolean;
  Mainnetbest : string;
  ErrorCode : String;
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
//ToLog('BESTHASH -> '+Data.Diff);

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
   //ToLog('MAINNET  -> '+Mainnetbest+' '+ErrorCode);
   if WasGood then
      begin
      ToLog(',Besthash submited: '+Data.Diff);
      Inc(SESSION_BestHashes);
      end;
   end
else
   begin
   ToLog('Unable to send solution');
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
   ToLog('Error saving share index: '+E.Message);
END;{TRY}
ShareIndexReport;
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
   ToLog('Error loading share index: '+E.Message);
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
   ToLog('Error crediting frequency: '+E.Message);
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
ToLog('Share index report created');
EXCEPT ON E:EXCEPTION do
   ToLog('Error saving sharereport.txt: '+E.message);
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
         SeekEof(ThisFile);
         end;
      end;
   CloseFile(ThisFile);
   EXCEPT ON E:EXCEPTION do
      ToLog('Error getting block value: '+E.Message);
   END;{TRY}
   end
else
   begin
   //ToLog('Block do not exists: '+FileName);
   end;
End;

Procedure FillSolsArray();
var
  counter        : integer;
  ThisBlockDiff  : string;
  ThisBlockValue : integer;
  LastBlock  : integer;
Begin
SetLength(ArraySols,0);
LastBlock := GetMyLastUpdatedBlock;
for counter := LastBlock-99 to LastBlock do
   begin
   ThisBlockDiff := GetBlockValue(counter,'SolDiff');
   if thisblockDiff = '' then thisblockDiff := DefWorst;
   ThisBlockValue := GetDiffHashrate(thisblockDiff);
   Insert(ThisBlockValue,ArraySols,length(ArraySols));
   end;
CalculateMainNetHashrate;
{
//ToLog('TotalValue: '+TotalValue.ToString);
//ToLog('Processed: '+Processed.ToString);
TotalRate := (TotalValue/100)/Processed;
//ToLog('TotalRate: '+TotalRate.ToString);
TotalRate := Power(16,TotalRate);
//ToLog('TotalRate: '+TotalRate.ToString);
TotalRate := TotalRate/(575);
MainnetHashRate := Round(TotalRate);
ToLog('Mainnet hashrate: '+HashrateToShow(MainnetHashRate));
}
End;

Procedure CalculateMainNetHashrate();
var
  counter        : integer;
  TotalValue     : integer = 0;
  TotalRate : double = 0;
Begin
for counter := 0 to length(ArraySols)-1 do
   TotalValue := TotalValue+ArraySols[counter];
//ToLog('Registers: '+length(ArraySols).ToString);
TotalRate := (TotalValue/100)/length(ArraySols);
//ToLog('TotalRate: '+TotalRate.ToString);
TotalRate := Power(16,TotalRate);
TotalRate := TotalRate/(575);
MainnetHashRate := Round(TotalRate);
End;

END. // End unit

