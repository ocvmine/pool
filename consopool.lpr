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
      Write(Format(' %d [%d/%d] ',[MainConsensus.block,ContactedNodes,LengthNodes]));
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
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(green);Write(BlockPrefixesRequested.ToString);
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(green);Write(MinersCount.ToString);
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(green);Write(SharesCount.ToString);
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(green);Write(Copy(ThisBlockBest,1,10));
   UpdateServerInfo := false;
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(green);Write(Copy(MainBestDiff,1,10));
   PrintLine(10);
   end;
if number = 6 then
   begin
   Textcolor(white);TextBackground(Black);
   write(Format(' %s    %d    %d    %s',[UpTime,SESSION_BestHashes, SESSION_Shares, GetSessionSpeed]));
   PrintLine(10);
   RefreshUpTime := UTCTime;
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
WriteLn('[Shortcut key]');
WriteLn();
Writeln('help    [F1]           -> Shows this info');
Writeln('nodes   [F2]           -> Shows the seed nodes');
Writeln('sync    [F3]           -> Syncs with mainnet (Debug)');
Writeln('run     [F5]           -> Starts the pool');
Writeln('stop    [F6]           -> Stops the pool');
Writeln('exit    [ESC]          -> Close the app');
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

For counter := 0 to LengthNodes-1 do
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

Procedure ShowBlockShares();
Var
  Counter : integer;
  ThisMiner : TMinersData;
Begin
OnMainScreen := false;
TextBackground(Black);TextColor(White);ClrScr();
PrintLine(1);WriteLn();
TextBackground(Black);TextColor(White);
WriteLn();
WriteLn('Nodes List: ');
WriteLn();
EnterCriticalSection(CS_Miners);
For counter := 0 to Length(ArrMiners)-1 do
   begin
   ThisMiner := ArrMiners[Counter];
   writeln(Format(' %0:-40s %d ',[ThisMiner.address,ThisMiner.Shares]));
   end;
LeaveCriticalSection(CS_Miners);
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

Procedure CheckLogs();
var
  Texto : String;
Begin
if length(LogLines)>0 then
   begin
   EnterCriticalSection(CS_LogLines);
   window(1,13,80,24);
   GotoXy(80,11);WriteLn();
   Repeat
      begin
      Texto := LogLines[0];
      Delete(LogLines,0,1);
      if Texto[1] = ',' then
         begin
         TextColor(Green);
         Texto := Copy(Texto,2,length(Texto));
         end
      else TextColor(White);
      WriteLn(Texto);
      end;
   until length(LogLines) = 0;
   Window(1,1,80,25);
   LeaveCriticalSection(CS_LogLines);
   end;
End;

BEGIN
InitCriticalSection(CS_UpdateScreen);
InitCriticalSection(CS_PrefixIndex);
InitCriticalSection(CS_LogLines);
InitCriticalSection(CS_Miners);
InitCriticalSection(CS_Shares);
InitCriticalSection(CS_BlockBest);
InitCriticalSection(CS_Solution);
SetLength(LogLines,0);
SetLength(NewLogLines,0);
SetLength(ArrMiners,0);
SetLength(ArrShares,0);
ClrScr;
if not directoryexists('logs') then createdir('logs');
if not directoryexists('miners') then createdir('miners');
AssignFile(MinersFile,'miners'+DirectorySeparator+'miners.dat');
Assignfile(configfile, 'consopool.cfg');
Assignfile(logfile, 'logs'+DirectorySeparator+'log.txt');
Assignfile(OldLogFile, 'logs'+DirectorySeparator+'oldlogs.txt');
if not FileExists('miners'+DirectorySeparator+'miners.dat') then CreateMinersFile();
LoadMiners();
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
ToLog('********** New Session **********');
REPEAT
   REPEAT
      If ((GetSolution.Diff<MainBestDiff) and (GetSolution.Hash<>'')) then SendSolution(GetSolution);
      if UpdateScreen then PrintUpdateScreen();
      CheckLogs();
      if ( ((LastConsensusTry+4<UTCTime) and (UTCTime-MainConsensus.LBTimeEnd>604) or (LastConsensusTry=0)) and
         (not WaitingConsensus) )then
         Begin
         PrintLine(4,'1');
         WaitingConsensus := true;
         CurrentBlock := GetMainConsensus.block;
         GetConsensus;
         if GetMainConsensus.block>CurrentBlock then
            begin
            CurrentBlock := GetMainConsensus.block;
            ResetBlock();
            UpdateServerInfo := true;
            end;
         WaitingConsensus := False;
         LastConsensusTry := UTCTime;
         PrintLine(4);
         End;
      if RefreshUpTime <> UTCtime then PrintLine(6);
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
      else if Uppercase(Parameter(Command,0)) = 'BLOCKSHARES' then ShowBlockShares
      else if Uppercase(Parameter(Command,0)) = 'PREFIX' then   // Debug only
         Begin
         ToLog(GetPrefixStr(StrToIntDef(Parameter(Command,1),0)));
         end
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
writeln();
PoolServer.Free;
DoneCriticalSection(CS_UpdateScreen);
DoneCriticalSection(CS_PrefixIndex);
DoneCriticalSection(CS_LogLines);
DoneCriticalSection(CS_Miners);
DoneCriticalSection(CS_Shares);
DoneCriticalSection(CS_BlockBest);
DoneCriticalSection(CS_Solution);
END.

