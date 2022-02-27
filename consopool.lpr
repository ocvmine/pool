PROGRAM consopool;

{$mode objfpc}{$H+}

USES
  {$IFDEF UNIX}
   cthreads,
  {$ENDIF}
  Classes, SysUtils, CRT, consopooldata, coreunit
  { you can add units after this };

Procedure PrintLine(number:integer;IfText:String='');
Begin
TextBackground(Black);
GotoXY(1,number);ClrEOL;
if number = 1 then
   begin
   Textcolor(Blue);TextBackground(White);
   Write(Format(' Noso pool Nosohash %s [FPC=%s] ',[AppVersion,fpcVersion]));
   end;
IF ((number > 1) and (not OnMainScreen)) then exit;
if number = 2 then
   begin
   Textcolor(white);TextBackground(Green);Write(Format(' %d ',[PoolPort]));
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(Green);Write(Format(' %s%% ',[FormatFloat('0.00',PoolFee/100)]));
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(Green);Write(Format(' %d ',[PoolPay]));
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(Green);Write(Format(' %s ',[MinDiffBase]));
   end;
if number = 3 then
   begin
   Textcolor(White);TextBackground(Blue);
   Write(Format(' %s ',[PoolAddress]));
   end;
if number = 4 then
   begin
   if IfText='1' then
      begin
      Textcolor(Red);TextBackground(black);
      Write(Format(' %s ',['Syncing']));
      end;
   if IfText<>'1' then
      begin
      Textcolor(white);TextBackground(green);
      Write(Format(' %d [%d/7] ',[MainConsensus.block,ContactedNodes]));
      TextBackground(Black);Write('  ');
      Textcolor(white);TextBackground(green);
      Write(Format(' %s ',[Copy(MainConsensus.lbhash,1,10)]));
      TextBackground(Black);Write('  ');
      Textcolor(white);TextBackground(green);
      Write(Format(' %d ',[UTCTime-MainConsensus.LBTimeEnd]));
      PrintLine(10);
      end;
   RefreshAge := UTCTime;
   end;
if number = 5 then
   begin
   if PoolServer.Active then
      begin
      TextColor(yellow);TextBackground(Green);Write(Format(' %s ',['( (LISTENING) )']))
      end
   else
      begin
      TextColor(White);TextBackground(Red);Write(Format(' %s ',['OFF']))
      end;
   UpdateServerInfo := false;
   PrintLine(10);
   end;
if number = 10 then
   begin
   Textcolor(white);TextBackground(Black);
   Write('> '+Command);
   end;
if number = 11 then
   begin
   Textcolor(black);TextBackground(white);
   Write(Format(' %s ',[IfText]));
   LastHelpShown := IfText;
   end;
End;

Procedure ShowHelp();
Begin
OnMainScreen := false;
TextBackground(Black);TextColor(White);ClrScr();
PrintLine(1);WriteLn();
TextBackground(Black);TextColor(White);
WriteLn();
WriteLn('Available commands (case unsensitive): ');
WriteLn();
Writeln('help                   -> Shows this info');
Writeln('exit                   -> Close the app');
WriteLn();Write('Press any key to return');
ThisChar := ReadKey;
If ThisChar = #0 then ThisChar := Readkey;
ClrScr();
OnMainScreen := true;
LastHelpShown := DefHelpLine;
SetUpdateScreen();
End;

Procedure ShowNodes();
Var
  Counter : integer;
  ThisNode : TNodeData;
Begin
OnMainScreen := false;
TextBackground(Black);TextColor(White);ClrScr();
PrintLine(1);WriteLn();
TextBackground(Black);TextColor(White);
WriteLn();
WriteLn('Nodes List: ');
WriteLn();

For counter := 0 to length(NodesArray)-1 do
   begin
   ThisNode := GetNodeIndex(Counter);
   writeln(Format(' %d %s %d',[Counter,ThisNode.host,ThisNode.port]));
   end;

WriteLn();
Write('Press any key to return');
ThisChar := ReadKey;
If ThisChar = #0 then ThisChar := Readkey;
ClrScr();
OnMainScreen := true;
LastHelpShown := DefHelpLine;
SetUpdateScreen();
End;

Procedure PrintUpdateScreen();
Begin
PrintLine(1);
PrintLine(2);
PrintLine(3);
PrintLine(4);
PrintLine(5);
PrintLine(11,LastHelpShown);
PrintLine(10);
End;

BEGIN
InitCriticalSection(CS_UpdateScreen);
InitCriticalSection(CS_PrefixIndex);
ClrScr;
if not directoryexists('logs') then createdir('logs');
Assignfile(configfile, 'consopool.cfg');
Assignfile(logfile, 'logs'+DirectorySeparator+'log.txt');
Assignfile(OldLogFile, 'logs'+DirectorySeparator+'oldlogs.txt');
If not ResetLogs then
   begin
   writeln('Error reseting log files');
   Exit;
   end;
if not FileExists('consopool.cfg') then SaveConfig();
LoadConfig();
LoadNodes;
InitServer;
MainConsensus := Default(TNodeData);
LastHelpShown := DefHelpLine;
REPEAT
   REPEAT
      if UpdateScreen then PrintUpdateScreen();
      if ( ((LastConsensusTry+4<UTCTime) and (UTCTime-MainConsensus.LBTimeEnd>604) or (LastConsensusTry=0)) and
         (not WaitingConsensus) )then
         Begin
         PrintLine(4,'1');
         WaitingConsensus := true;
         GetConsensus;
         WaitingConsensus := False;
         LastConsensusTry := UTCTime;
         PrintLine(4);
         End;
      if RefreshAge<> UTCTime then
         begin
         PrintLine(4);
         end;
      if UpdateServerInfo then PrintLine(5);
      Sleep(1);
   UNTIL Keypressed;
   ThisChar := Readkey;
   if ThisChar = #0 then
      begin
      ThisChar:=Readkey;
      if ThisChar=#59 then // F1
         begin
         Command := 'help';
         ThisChar := #13;
         end;
      if ThisChar=#60 then // F2
         begin
         Command := 'nodes';
         ThisChar := #13;
         end;
      if ThisChar=#61 then // F3
         begin
         Command := 'sync';
         ThisChar := #13;
         end;
      end;
   if ((Ord(ThisChar)>=32) and (Ord(ThisChar)<=126)) then
      begin
      Command := Command+ThisChar;
      PrintLine(10);
      end
   else if Ord(ThisChar) = 8 then
      begin
      SetLength(Command,Length(Command)-1);
      PrintLine(10);
      end
   else if Ord(ThisChar) = 13 then
      begin
      if Uppercase(Parameter(Command,0)) = 'EXIT' then FinishProgram := true
      else if Uppercase(Parameter(Command,0)) = 'HELP' then ShowHelp
      else if Uppercase(Parameter(Command,0)) = 'NODES' then ShowNodes
      else if Uppercase(Parameter(Command,0)) = 'RUN' then PrintLine(11,StartPool)
      else if Uppercase(Parameter(Command,0)) = 'STOP' then PrintLine(11,StopPool)
      else if Uppercase(Parameter(Command,0)) = 'SYNC' then
         begin
         PrintLine(4,'1');
         WaitingConsensus := true;
         GetConsensus;
         WaitingConsensus := False;
         LastConsensusTry := UTCTime;
         PrintLine(4);
         end
      else if Command <> '' then PrintLine(11,' Error.'+DefHelpLine);
      Command :='';
      PrintLine(10);
      end
   else if Ord(ThisChar) = 27 then FinishProgram := true;
UNTIL FinishProgram;
DoneCriticalSection(CS_UpdateScreen);
DoneCriticalSection(CS_PrefixIndex);
END.

