UNIT consopooldata;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, coreunit, IdTCPServer, IdContext, IdGlobal, StrUtils;

Type
  PoolServerEvents = class
    class procedure OnExecute(AContext: TIdContext);
    class procedure OnConnect(AContext: TIdContext);
  end;

  TMinersData = Packed Record
    address : string[40];
    Balance : int64;
    LastPay : integer;
    end;

Procedure CreateMinersFile();
Procedure LoadMiners();
Function MinersCount():integer;
Function GetMinerBalance(address:string):int64;

Function ShareAlreadyExists(Share:string):boolean;
Function ShareIsValid(Share,Address:String):Boolean;

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
Function StopPool():String;
Procedure ToLog(Texto:string);
Procedure ResetBlock();
Function GetPrefixIndex():Integer;
Procedure ResetPrefixIndex();
function GetPrefixStr():string;

CONST
  fpcVersion = {$I %FPCVERSION%};
  AppVersion = 'v0.1';
  DefHelpLine= 'Type help for available commands';

VAR
  // Files
  configfile, LogFile, OldLogFile : TextFile;
  MinersFile : File of TMinersData;
  // Config values
  PoolPort     : integer = 8082;
  MinDiffBase  : string  = '00000';
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
  LogLines     : Array of String;
  NewLogLines  : Array of string;
  // Mainnet
  LastConsensusTry: int64   = 0;
  WaitingConsensus:Boolean = false;
  // Pool
  PoolServer : TIdTCPServer;
  PrefixIndex: Integer = 0;
  MinerDiff  : String = '';
  IPMiners: integer = 100;
  ArrMiners : Array of TMinersData;
  ArrShares : Array of string;

  // Critical sections
  CS_UpdateScreen : TRTLCriticalSection;
  CS_PrefixIndex  : TRTLCriticalSection;
  CS_NewLogLines  : TRTLCriticalSection;
  CS_LogLines     : TRTLCriticalSection;
  CS_Miners       : TRTLCriticalSection;
  CS_Shares       : TRTLCriticalSection;

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

Function ShareIsValid(Share,Address:String):Boolean;
var
  ThisHash : string;
Begin
Result := true;
if ShareAlreadyExists(Share+address) then Result := false
else
   begin
   ThisHash := NosoHash(Share+Address);
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
ResetBlock();
EXCEPT ON E:Exception do
   Result := E.Message;
END; {TRY}
If success then
   Begin
   Result := 'Pool server started';
   UpdateServerInfo := true;
   end;
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
EXCEPT On E:Exception do
   Result := E.Message;
END; {TRY}
If success then
   Begin
   Result := 'Pool stopped';
   UpdateServerInfo := true;
   end;
End;

Procedure ToLog(Texto:string);
Begin
Texto := DateTimeToStr(now)+' '+Texto;
EnterCriticalSection(CS_NewLogLines);
Insert(Texto,NewLogLines,Length(NewLogLines));
LeaveCriticalSection(CS_NewLogLines);
EnterCriticalSection(CS_LogLines);
Insert(Texto,LogLines,Length(LogLines));
LeaveCriticalSection(CS_LogLines);
if OnLogScreen then WriteLn(Texto);
End;

Procedure ResetBlock();
Begin
ResetPrefixIndex();
SetLength(ArrShares,0);
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

function GetPrefixStr():string;
var
  Index, firstchar, secondchar, ThirdChar : integer;
  HashChars : integer;
Begin
Index := GetPrefixIndex();
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
Linea := AContext.Connection.IOHandler.ReadLn('',1000,-1,IndyTextEncoding_UTF8);
EXCEPT On E:Exception do
   begin
   TRY
   AContext.Connection.Disconnect;
   EXCEPT On E:Exception do DoNothing;
   END{Try};
   end;
END{Try};
Command := Parameter(Linea,0);
Address := Parameter(Linea,1);
If UpperCase(Command) = 'SOURCE' then
   begin
   if CheckIPMiners(IPUser) then
      begin
      TryClosePoolConnection(AContext,GetPrefixStr+' '+MinerDiff+' '+GetMinerBalance(Address).ToString);
      end;
   end;
If UpperCase(Command) = 'SHARE' then
   begin
   ThisShare := Parameter(Linea,2);
   if ShareIsValid(ThisShare,Address) then
      begin

      end;
   end;
End;

END. // End unit

