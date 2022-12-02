unit NosoTime;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IdSNTP, DateUtils, coreunit {use here the unit with parameter};

Type
   TThreadUpdateOffeset = class(TThread)
   private
     Hosts: string;
   protected
     procedure Execute; override;
   public
     constructor Create(const CreatePaused: Boolean; const THosts:string);
   end;

Function GetNetworkTimestamp(hostname:string):int64;
function TimestampToDate(timestamp:int64):String;
Function GetTimeOffset(NTPServers:String):int64;
Function UTCTime:Int64;
Procedure UpdateOffset(NTPServers:String);

Var
  LocalVar_TimeOffset : int64 = 0;

IMPLEMENTATION

constructor TThreadUpdateOffeset.Create(const CreatePaused: Boolean; const THosts:string);
begin
  inherited Create(CreatePaused);
  Hosts := THosts;
end;

procedure TThreadUpdateOffeset.Execute;
var
  TimeToRun : int64;
  TFinished  : boolean = false;
Begin
GetTimeOffset(Hosts);
End;

Function GetNetworkTimestamp(hostname:string):int64;
var
  NTPClient: TIdSNTP;
begin
result := 0;
NTPClient := TIdSNTP.Create(nil);
   TRY
   NTPClient.Host := hostname;
   NTPClient.Active := True;
   NTPClient.ReceiveTimeout:=500;
   result := DateTimeToUnix(NTPClient.DateTime);
   if result <0 then result := 0;
   EXCEPT on E:Exception do
      result := 0;
   END; {TRY}
NTPClient.Free;
end;

function TimestampToDate(timestamp:int64):String;
begin
result := DateTimeToStr(UnixToDateTime(TimeStamp));
end;

Function GetTimeOffset(NTPServers:String):int64;
var
  ThisParam : string  = '';
  Counter   : integer = 0;
  ThisNTP   : int64;
Begin
Result := 0;
NTPServers := StringReplace(NTPServers,':',' ',[rfReplaceAll, rfIgnoreCase]);
NTPServers := Trim(NTPServers);
Repeat
   ThisParam := Parameter(NTPServers,counter);
   if ThisParam <> '' then
      begin
      ThisNTP := GetNetworkTimestamp(ThisParam);
      if ThisNTP>0 then
         begin
         Result := ThisNTP - DateTimeToUnix(Now);
         break;
         end;
      end;
   Inc(Counter);
until thisParam = '';
LocalVar_TimeOffset := Result;
End;

Function UTCTime:Int64;
Begin
Result := DateTimeToUnix(Now, False) +LocalVar_TimeOffset;
End;

Procedure UpdateOffset(NTPServers:String);
var
  LThread : TThreadUpdateOffeset;
Begin
LThread := TThreadUpdateOffeset.Create(true,NTPservers);
LThread.FreeOnTerminate:=true;
LThread.Start;
End;

END. // END UNIT

