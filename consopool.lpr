PROGRAM consopool;

{$mode objfpc}{$H+}

USES
  {$IFDEF UNIX}
   cthreads,
  {$ENDIF}
  Classes, SysUtils, CRT, consopooldata, coreunit, NosoDig.Crypto
  { you can add units after this };

// Prints the specified line of the screen
Procedure PrintLine(number:integer;IfText:String='');
var
  BlockAge : int64 = 0;
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
   if AutoDiff then TextBackground(Green) else TextBackground(Red);
   Textcolor(white);Write(Format(' %s [%d] ',[MinDiffBase, AutoValue]));
   end;
if number = 3 then
   begin
   Textcolor(White);TextBackground(Blue);
   Write(Format(' %s [ %s Noso ]',[PoolAddress,Int2Curr(GetPoolBalance)]));
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
      BlockAge := UTCTime-MainConsensus.LBTimeEnd;
      Write(Format(' %d ',[BlockAge]));
      if ( (BlockAge > 60) and (LastPaidBlock<MainConsensus.block) ) then
         begin
         RunPayments();
         LastPaidBlock:=MainConsensus.block;
         end;
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
   Textcolor(white);TextBackground(green);Write(BestHashReadeable(ThisBlockBest));
   UpdateServerInfo := false;
   TextBackground(Black);Write('  ');
   Textcolor(white);TextBackground(green);Write(BestHashReadeable(MainBestDiff));
   PrintLine(10);
   end;
if number = 6 then
   begin
   Textcolor(white);TextBackground(Black);
   write(Format(' %s  %d  %d  %s  [%s]  [%d]',[UpTime,SESSION_BestHashes, SESSION_Shares, HashrateToShow(GetSessionSpeed),HashrateToShow(MainNetHashRate),BlocksMinedByPool]));
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
   writeln(Format(' %2s %18s %s %6s %d',[Counter.ToString,ThisNode.host,ThisNode.port.ToString,ThisNode.block.ToString,ThisNode.LBPoW,MainConsensus.block]));
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
WriteLn('Block shares: ');
WriteLn();
EnterCriticalSection(CS_Miners);
For counter := 0 to Length(ArrMiners)-1 do
   begin
   ThisMiner := ArrMiners[Counter];
   writeln(Format(' %0:-40s %12s %5s %d',[ThisMiner.address,Int2Curr(ThisMiner.Balance),ThisMiner.Shares.ToString,MainConsensus.block-ThisMiner.LastPay]));
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
   window(1,13,80,24);
   GotoXy(80,11);WriteLn();
   Repeat
      begin
      EnterCriticalSection(CS_LogLines);
      Texto := LogLines[0];
      Delete(LogLines,0,1);
      LeaveCriticalSection(CS_LogLines);
      if Texto[1] = ',' then
         begin
         TextColor(Green);
         Texto := Copy(Texto,2,length(Texto));
         end
      else if Texto[1] = '.' then
         begin
         TextColor(red);
         Texto := Copy(Texto,2,length(Texto));
         end
      else if Texto[1] = '/' then
         begin
         TextColor(yellow);
         Texto := Copy(Texto,2,length(Texto));
         end
      else TextColor(White);
      WriteLn(Texto);
      RawToLog(Texto);
      end;
   until length(LogLines) = 0;
   Window(1,1,80,25);
   end;
End;

Procedure CloseTheApp(mensaje:string);
Begin
if mensaje<>'' then
   begin
   writeln(mensaje);
   writeln('Press enter to close');
   Readln;
   end;
PoolServer.Free;
DoneCriticalSection(CS_UpdateScreen);
DoneCriticalSection(CS_PrefixIndex);
DoneCriticalSection(CS_LogLines);
DoneCriticalSection(CS_Miners);
DoneCriticalSection(CS_Shares);
DoneCriticalSection(CS_BlockBest);
DoneCriticalSection(CS_Solution);
DoneCriticalSection(CS_PaysFile);
DoneCriticalSection(CS_PayThreads);
DoneCriticalSection(CS_PoolBalance);
DoneCriticalSection(CS_LastBlockRate);
Halt(2);
End;

BEGIN
InitCriticalSection(CS_UpdateScreen);
InitCriticalSection(CS_PrefixIndex);
InitCriticalSection(CS_LogLines);
InitCriticalSection(CS_Miners);
InitCriticalSection(CS_Shares);
InitCriticalSection(CS_BlockBest);
InitCriticalSection(CS_Solution);
InitCriticalSection(CS_PaysFile);
InitCriticalSection(CS_PayThreads);
InitCriticalSection(CS_PoolBalance);
InitCriticalSection(CS_LastBlockRate);

SetLength(LogLines,0);
SetLength(NewLogLines,0);
SetLength(ArrMiners,0);
SetLength(ArrShares,0);
ClrScr;
if not directoryexists('logs') then createdir('logs');
if not directoryexists('miners') then createdir('miners');
if not directoryexists('blocks') then createdir('blocks');
if not directoryexists('addresses') then createdir('addresses');
AssignFile(MinersFile,'miners'+DirectorySeparator+'miners.dat');
//*****
//AssignFile(TempMinersFile ,'miners'+DirectorySeparator+'miners2.dat');
//*****
Assignfile(configfile, 'consopool.cfg');
Assignfile(logfile, 'logs'+DirectorySeparator+'log.txt');
Assignfile(OldLogFile, 'logs'+DirectorySeparator+'oldlogs.txt');
if not FileExists('blocks'+DirectorySeparator+'0.txt') then CreateBlockzero();
if not fileExists('payments.txt') then createPaymentsFile;
if not fileExists('frequency.dat') then SaveShareIndex;
if StoreShares then LoadShareIndex;
Assignfile(PaysFile,'payments.txt');
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

if PoolAddress='' then CloseTheApp('Pool address is empty');
if PublicKey  ='' then CloseTheApp('Public key is empty');
if PrivateKey  ='' then CloseTheApp('Private key is empty');
if not IsValidHashAddress(PoolAddress) then CloseTheApp('Pool address is not valid');
if GetAddressFromPublicKey(PublicKey) <> PoolAddress then CloseTheApp('Address and public key do not match');
if not KeysMatch(PublicKey,PrivateKey) then CloseTheApp('Keys do not match');

MainnetTimeStamp := GetMainnetTimestamp;
if MainnetTimeStamp<>0 then OffSet := UTCTime-MainnetTimeStamp;
MainConsensus := Default(TNodeData);
LastHelpShown := DefHelpLine;
UpdatePoolBalance;
FillSolsArray();
ToLog('********** New Session **********');
if PoolAuto then PrintLine(11,StartPool);
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
      if RefreshPoolHeader then
         begin
         RefreshPoolHeader := false;
         PrintLine(2);
         end;
      if RefreshPoolBalance then
         begin
         RefreshPoolBalance := false;
         PrintLine(3);
         end;
      if RefreshAge<> UTCTime then
         begin
         PrintLine(4);
         if ( (CheckPaysThreads) and (GetPayThreads= 0) ) then
            begin
            CheckPaysThreads := false;
            SaveMiners();
            ToLog(Format('Completed payments (%d Good - %d Fail)',[GoodPayments,BadPayments]));
            GenerateReport();
            UpdatePoolBalance;
            end;
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
      else if Uppercase(Parameter(Command,0)) = 'SHARES' then ShowBlockShares
      else if Uppercase(Parameter(Command,0)) = 'LB' then ToLog(GetMyLastUpdatedBlock.ToString)
      else if Uppercase(Parameter(Command,0)) = 'SHAREINDEX' then ShareIndexReport
      else if Uppercase(Parameter(Command,0)) = 'NETRATE' then FillSolsArray
      else if Uppercase(Parameter(Command,0)) = 'POOLINFO' then
         ToLog(minerscount.ToString+' '+GetLastBlockRate.ToString+' '+PoolFee.ToString)

      else if Uppercase(Parameter(Command,0)) = 'SAVE' then
         begin
         SaveMiners;
         ToLog('Miners File saved');
         end

      else if Uppercase(Parameter(Command,0)) = 'TESTPAY' then
         begin
         EnterCriticalSection(CS_Miners);
         ArrMiners[0].Balance:=ArrMiners[0].Balance+1000;
         LeaveCriticalSection(CS_Miners);
         end
      else if Uppercase(Parameter(Command,0)) = 'DEBT' then ToLog('Total debt: '+Int2Curr(GetTotalDebt))
      else if Uppercase(Parameter(Command,0)) = 'REPORT' then GenerateReport
      else if Uppercase(Parameter(Command,0)) = 'BALANCE' then
         begin
         ToLog(Format('Pool Address: %s',[Int2Curr(GetAddressBalance(PoolAddress))]));
         end
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
   sleep(1);
UNTIL FinishProgram;
SaveMiners();
CloseTheApp('');
END.

