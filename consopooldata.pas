UNIT consopooldata;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, coreunit, IdTCPServer, IdContext, IdGlobal, StrUtils,
  IdTCPClient, math;

Type
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
    address : string[40];
    Balance : int64;
    LastPay : integer;
    Shares  : integer;
    end;

Procedure CreateMinersFile();
Procedure LoadMiners();
Function MinersCount():integer;
Function GetMinerBalance(address:string):int64;

Procedure AddShare(Share:string);
Function SharesCount():Integer;
Function ShareAlreadyExists(Share:string):boolean;
Procedure CreditShare(Address:String);
Function ShareIsValid(Share,Address:String):Boolean;

Procedure SetSolution(Data:TSolution);
Function GetSolution():TSolution;

Function ResetLogs():boolean;
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
Function GetSessionSpeed(): String;
Procedure ToLog(Texto:string);
Function GetBlockBest():String;
Procedure SetBlockBest(ThisValue:String);
Procedure ResetBlock();
Function GetPrefixIndex():Integer;
Procedure ResetPrefixIndex();
function GetPrefixStr(IndexValue:integer = -1):string;
Procedure SendSolution(Data:TSolution);

CONST
  fpcVersion = {$I %FPCVERSION%};
  AppVersion = 'v0.1';
  DefHelpLine= 'Type help for available commands';
  DefWorst = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';

VAR
  // Files
  configfile, LogFile, OldLogFile : TextFile;
  MinersFile : File of TMinersData;
  // Config values
  PoolPort     : integer = 8082;
  MinDiffBase  : string  = '0000';
  PoolFee      : integer = 100;
  PoolPay      : integer = 30;
  PoolAddress  : String  = 'N4PeJyqj8diSXnfhxSQdLpo8ddXTaGd';
  // Operative
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
  LogLines     : Array of String;
  NewLogLines  : Array of string;
  // Mainnet
  LastConsensusTry: int64   = 0;
  WaitingConsensus:Boolean = false;
  CurrentBlock    : integer = 0;
  MainBestDiff    : String = DefWorst;
  // Pool
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

IMPLEMENTATION

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

Procedure LoadMiners();
var
  ThisData : TMinersData;
Begin
reset(MinersFile);
While not eof(MinersFile) do
   begin
   read(MinersFile,ThisData);
   Insert(ThisData,ArrMiners,Length(ArrMiners));
   end;
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

Function ShareIsValid(Share,Address:String):Boolean;
var
  ThisHash, ThisDiff : string;
  ThisSolution : TSolution;
Begin
Result := False;
if ShareAlreadyExists(Share) then
   begin
   ToLog('Duplicated: '+share);
   end
else
   begin
   ThisHash := NosoHash(Share+PoolAddress);
   ThisDiff := CheckHashDiff(GetMainConsensus.LBHash,ThisHash);
   if ThisDiff < MinerDiff then // Valid share
      begin
      Result := true;
      AddShare(Share+Address);
      CreditShare(Address);
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
      //ToLog('Invalid share: '+Slinebreak+address+slinebreak+share+slinebreak+thisdiff+slinebreak+'TARGET:'+GetMainConsensus.LBHash);
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

// Creates/saves config data
Function SaveConfig():boolean;
Begin
result := true;
TRY
rewrite(configfile);
writeln(configfile,'pooladdress '+PoolAddress);
writeln(configfile,'poolport '+IntToStr(PoolPort));
writeln(configfile,'diffbase '+MinDiffBase);
writeln(configfile,'poolfee '+IntToStr(PoolFee));
writeln(configfile,'poolpay '+IntToStr(PoolPay));
writeln(configfile,'ipminers '+IntToStr(IPMiners));
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
   if uppercase(Parameter(linea,0)) = 'IPMINERS' then IPMiners := StrToIntDef(Parameter(linea,1),IPMiners);
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
if bestdiff[counter]='5' then Result := Result+3;
End;

Function GetSessionSpeed(): String;
var
  seconds : integer;
Begin
Seconds := UTCTime-SESSION_Started;
Result := IntToStr((SESSION_HashPerShare*SESSION_Shares) div (seconds+1))+' Kh/s';
End;

Procedure ToLog(Texto:string);
Begin
if Texto[1] = ',' then
   begin
   Texto := copy(Texto,2, length(texto));
   Texto := ','+DateTimeToStr(now)+' '+Texto;
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

Procedure ResetBlock();
Begin
MainBestDiff := DefWorst;
ResetPrefixIndex();
SetBlockBest(DefWorst);
SetSolution(Default(TSolution));
EnterCriticalSection(CS_Shares);
SetLength(ArrShares,0);
LeaveCriticalSection(CS_Shares);
SetLength(ArrMiners,0);
BlockPrefixesRequested := 0;
End;

Function GetPrefixIndex():Integer;
Begin
EnterCriticalSection(CS_PrefixIndex);
Result := PrefixIndex;
Inc(PrefixIndex);
LeaveCriticalSection(CS_PrefixIndex);
End;

Procedure ResetPrefixIndex();
Begin
EnterCriticalSection(CS_PrefixIndex);
PrefixIndex := 0;
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
//PoolServer.Contexts.LockList;
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
      Inc(BlockPrefixesRequested);
      // 1{MinerPrefix} 2{MinerAddress} 3{PoolMinDiff} 4{LBHash} 5{LBNumber} 6{MinerBalance}
      //ToLog('Miner from '+IPUser);
      TryClosePoolConnection(AContext,'OK '+{1}GetPrefixStr+' '+
                                            {2}PoolAddress+' '+
                                            {3}MinerDiff+' '+
                                            {4}GetMainConsensus.LBHash+' '+
                                            {5}GetMainConsensus.block.ToString+' '+
                                            {6}GetMinerBalance(Address).ToString);
      end;
   end
else If UpperCase(Command) = 'SHARE' then
   begin
   ThisShare := Parameter(Linea,2);
   if ShareIsValid(ThisShare,Address) then
      begin
      TryClosePoolConnection(AContext,'True');
      //ToLog('Valid share from '+Address);
      end
   else
      begin
      TryClosePoolConnection(AContext,'False');
      ToLog('Wrong share from '+Address);
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
ToLog('BESTHASH -> '+Data.Diff);

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
   ToLog('MAINNET  -> '+Mainnetbest+' '+ErrorCode);
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

END. // End unit

