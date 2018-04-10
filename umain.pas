(*
 This file is part of TrayTrend.

 TrayTrend is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 TrayTrend is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with TrayTrend.  If not, see <http://www.gnu.org/licenses/>.

 (c) 2018 Björn Lindh - https://github.com/slicke/traytrend
*)
unit umain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  intfgraphics, LCLType, StdCtrls, Buttons,
  fpImage,  fphttpclient, sha1, fpjson, dateutils, jsonconf,
  lazutf8sysutils, uconfig, usys, lclintf, Menus, uhover
  {$ifdef Windows}, mmsystem, Comobj, urlmon,{$endif}{$ifdef Darwin}, speechsynthesizer, MacOSAll, appkit, CarbonGDIObjects, carbonmenus,{$ENDIF}
  Classes, stuff;

type

  {$ifdef Darwin}
    {$define OSBitmap}
  {$endif}
  TOSImg = {$ifdef OSBitmap}TBitmap{$else}TIcon{$endif};

  // Settings stored in the config file
  TUserVals = record
    ok, hypo, hyper: single;
    cok, chypo, chyper, csoonhyper: tcolor;
    url, api, lowexec, sndhyper, sndhypo: string;
    mmol, alert, colorval, colortrend, hover, hovercolor, hoverwindowcolor, voice, voiceall, voicetrend: boolean;
    snooze, arrows, hovertrans, updates: integer;
    mac_dock, mac_bounce, mac_bounce_once: boolean;
  end;

  // NightScout's possible directions/trends. Ported from the NS server source code.
  TBGTrend = (NONE, DoubleUp, SingleUp, FortyFiveUp, Flat, FortyFiveDown, SingleDown, DoubleDown, NOT_COMPUTABLE, RATE_OUT_OF_RANGE, NO_DATA);

  { TfMain }

  TfMain = class(TForm)
    btConf: TBitBtn;
    btnUpdate: TBitBtn;
    btOS: TBitBtn;
    ilBG: TImageList;
    ilFull: TImageList;
    imMacDock: TImage;
    imTrend: TImage;
    lblSnooze: TLabel;
    lblSpeed: TLabel;
    lblTimeAgo: TLabel;
    lblTrend: TLabel;
    lblVal: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miExit: TMenuItem;
    miAbout: TMenuItem;
    miTrend: TMenuItem;
    Panel1: TPanel;
    pnMain: TPopupMenu;
    pnTop: TPanel;
    tUpdate: TTimer;
    tTray: TTrayIcon;
    procedure btnUpdateClick(Sender: TObject);
    procedure btConfClick(Sender: TObject);
    procedure btOSClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure miAboutClickA(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure tTrayClick(Sender: TObject);
    procedure tUpdateTimer(Sender: TObject);
    procedure UpdateTrend(velocity: single; desc, device: string; newdate: tdatetime);


   {$IFDEF LCLCarbon}
     procedure SetOverlay(trend: HBitmap);
   {$ENDIF}
  private
    procedure FetchValues;
    function CheckVesion(current: Single; prerelease: boolean): boolean;
    function GetMetric(metric: string): TJSONData;
  public
    procedure UpdateBG;
    function SetUI(bgval: single; title: string; lbl: tlabel; img: tosimg; smallimg: ticon): tcolor;
    function FormatBG(val: single; short: boolean): string;
    function ConvertBGUnit(val: single; ismmol: boolean): single;
    procedure LoadCFG;
    function GetBGColor(val: single): tcolor;
    function GetTrendName(tr: TBGTrend): string;
  end;

const
  ttversion = 1.91;
  ttversionpre = true;
  ttversionpretty = ('Pre-release 2-r1 (internal: 1.91)');
var
  fMain: TfMain;
  cfg: TUserVals;                  // Current config variables loaded
  lastbg: single = -1;             // Last processed blood sugar
  bgval: single;                   // Current blood sugar
  lastread: int64;                 // Timestamp for last reading (as reported by NS)
  bgtrend: string;                 // Current trend
  lastalert: TBGTrend = NONE;      // The trend when last alert was triggered
  lastalertts: TDateTime;          // Time and date when last alert was triggered
  lastbgtrend: TBGTrend;           // Last processed trend
{$ifdef Darwin}
  macmenu: tmainmenu;
  applemenu: tmenuitem;
  macabout: tmenuitem;
  macsettings: tmenuitem;
  maccustomization: tmenuitem;
{$endif}
implementation

// Process a trend to a GUI string
function tfMain.GetTrendName(tr: TBGTrend): string;
begin
  case tr of
    NONE:
      result := 'No trend';
    DoubleUp:
      result := 'Very fast rise';
    SingleUp:
      result := 'Fast rise';
    FortyFiveUp:
      result := 'Rising';
    Flat:
      result := 'Steady';
    FortyFiveDown:
      result := 'Decline';
    SingleDown:
      result := 'Fast decline';
    DoubleDown:
      result := 'Very fast decline';
    NOT_COMPUTABLE:
      result := 'Not computable by NightScout';
    RATE_OUT_OF_RANGE:
      result := 'Rate out of range';
    NO_DATA:
      result := 'No recent readings';
  end;
end;

// Get data from Nightscout
function tfMain.GetMetric(metric: string): TJSONData;
var
  ans : string;
  code: integer;
begin
  try
   with TFPHTTPClient.Create(nil) do
   try


     AddHeader('API-SECRET', SHA1Print(SHA1String(cfg.api)));
     ans := Get(cfg.url + '/api/v1/'+metric+'.json');
     result := GetJSON(ans);
   finally
     code := ResponseStatusCode;
     Free;
   end;
  except
    on E: Exception do begin
    if code = 401 then
      ttMsg('Your credentials (API token) appears to be wrong, please verify your configuration. (Error 401)')
    else if code = 400 then
      ttMsg('Nightscout can''t understand our request. Either Nightscout is malfunctioning or you have not entered the address correctly. (Error 400)')
    else if code = 404 then
      ttMsg('No system appears to exist at the NightScout address you have specified. (Error 404)')
    else
       ttMsg('A netowork error occured: ' + E.Message + LineEnding + 'A new attempt will be made momentarily');
    end;
  end;
end;

// Check the application version
function tfMain.CheckVesion(current: Single; prerelease: boolean): boolean;
{$ifdef Windows}
const
  f1 = {$ifdef CPUX86_64}'ssleay32.dll'{$else}'libssl32.dll'{$endif};
  f2 = {$ifdef CPUX86_64}'libeay32.dll'{$else}'libeay32.dll'{$endif};
{$endif}
var
  ans, ver : string;
  res:TJSONData;
  tmpfs: TFormatSettings;
begin
  tmpfs.DecimalSeparator := '.';
  ver := FloatToStrF(current, ffGeneral, 3, 3, tmpfs);

  // We want to differentiate releases and pre builds
  if not prerelease then
    ver := 'v'+ver;
  ans := '';
  try

  with TFPHTTPClient.Create(nil) do begin
   AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb) TrayTrend/'+ver);                       // We don't get any result without the user agent set
   ans :=  Get('https://api.github.com/repos/slicke/traytrend/releases/latest');                   // As GitHub for the recent releases
  end;
  except {$ifdef Windows}
    on E: EInOutError do begin
      if messagedlg('Missing Windows components', 'Many, many programs need OpenSSL. Rcent Windows 10 versions now ship it aswell, but we couldn''t find it!'+
             'Download now?', mtWarning, [mbYes, mbAbort], 0, mbYes) = mrYes then begin
      if not UrlDownloadToFile(nil, PChar('http://www.miroslavnovak.com/totalcmd-openssl/64bit/'+f1), PChar(ExtractFileDir(Application.ExeName)+'\'+f1), 0, nil) = 0 then
        ShowMessage('We didn''t get it!');
      if not UrlDownloadToFile(nil, PChar('http://www.miroslavnovak.com/totalcmd-openssl/64bit/'+f2), PChar(ExtractFileDir(Application.ExeName)+'\'+f2), 0, nil) = 0 then
        ShowMessage('We didn''t get it!');
      end else
       ShowMessage('You will not be able to use https addresses!');
    end;
      {$endif}
 end;

  if ans = '' then
    Exit;

  if not prerelease then // So we dont have to parse the results differently, there's not array if we only request one entry
    ans := '['+ans+']';

  try
    res := GetJSON(ans);
    if (res.Items[0].FindPath('tag_name').AsString <> ver) then
          if MessageDlg('New version released', 'A new version of TrayTrend is available!'+LineEnding+LineEnding+'TrayTrend ' + res.Items[0].FindPath('tag_name').AsString+' has been released. You are currently using '+ver+'.'+LineEnding+'Would you like to get information about the new version?', mtConfirmation,  [mbYes, mbNo], 0) = mrYes then
               openurl(res.Items[0].FindPath('html_url').AsString);
  except
      ttMsg('Update check failed!', 'We were not able to check for updates! You should check out github.com/slicke/traytrend for updates!')
  end;
end;

// Get glucose values from NS
procedure tfMain.FetchValues;
var
  val, res: TJSONData;
  i: integer;
  ts: int64;
  tdate: TDateTime;
begin
  // Contact the API over SSL
  lblTimeAgo.Caption:= 'Getting new values';
  lblTimeAgo.Color := $006D686B;
  Application.ProcessMessages;
  res := GetMetric('entries');


  // Go through all resturned values in reverse order, so we enter them in chronological order
  for i := res.Count-1 downto 0 do begin
    val := res.Items[i];

    // Skip if the entry isn't a glucose reading
    if val.findpath('type').AsString <> 'sgv' then
      Continue;

    // Set the current BG as the last one, before reading a new "current"
    lastbg := bgval;
    bgval := val.findpath('sgv').AsInteger;
    ts := val.findpath('date').AsInt64;

    // If the last and current timestamps match nothing has changed
    if ts = lastread then
        Exit;
    lastread := ts;

    // We get milliseconds here, wehich we remove
    tdate := UnixToDateTime(round(ts/1000));
    UpdateTrend(val.findpath('delta').AsInteger, val.findpath('direction').AsString, val.findpath('device').AsString, tdate);
  end;
end;

// Process a new value/reading and put it in the GUI
procedure TfMain.UpdateTrend(velocity: single; desc, device: string; newdate: TDateTime);
var
  datediff: int64;
begin
  // Update labels with basic info
  bgtrend := desc;
  lblSpeed.Caption := FormatBG(velocity, false);

  // Add a + if the velocity is positive, minus is handled by conversion anyway
  if velocity > 0 then
    lblSpeed.Caption := '+'+lblSpeed.Caption;

   // Get a human readable value, minutes between readings
  datediff := MinutesBetween(NowUTC, newdate);

  // But we dont want to display 120 minutes, 2 hours is better etc
  if datediff <= 60 then begin
    lblTimeAgo.Caption := Format('%d min', [datediff]);
    lblTimeAgo.Color := clBlack;
  end else if datediff >= 1440 then begin
    lblTimeAgo.Caption := Format('%d day(s)', [DaysBetween(NowUTC, newdate)]);
    lblTimeAgo.Color := clRed;
  end else begin
    lblTimeAgo.Caption := Format('%d hrs', [HoursBetween(NowUTC, newdate)]);
    lblTimeAgo.Color := clMaroon;
  end;
// lblTimeAgo.Left := lblSpeed.Left-lblTimeAgo.Width-5;

  // We set the full time as a hint if one hovers the time label
  lblTimeAgo.Hint := DateTimeToStr(newdate);

  // If the user wants to run a process if low, we do it here
  if (bgval < 3) and (cfg.lowexec <> '') and (MinutesBetween(Now, lastalertts) >= cfg.snooze) then
    ExecuteProcess(Utf8ToAnsi(cfg.lowexec), '--bg-alert='+floattostr(bgval), []);
end;

// Process a reading into a good value.
function TfMain.FormatBG(val: single; short: boolean): string;
begin
  if cfg.mmol then
    result := FloatToStrF(ConvertBGUnit(val, false), ffFixed, 3, 1) // We get mg/dL from NS so we need to convert to mmol/L
  else
    result := FloatToStrF(val, ffNumber, 3, 0); // Just format the mg/dL value, no conversion needed

  if not short then begin  // Short determines if we add mmol/mg/dl at the end of the reading
    if cfg.mmol then
       result := Format('%s mmol/L', [result]) // See above, but with the unit added
    else
       result := Format('%s mg/dL', [result]);
  end;
end;

// Convert units between one and other
function TfMain.ConvertBGUnit(val: single; ismmol: boolean): single;
begin
if ismmol then // meaning we want mg/dl
   result := val* 18
else
  result := val/18;
end;

// (Re)Load the config file
procedure TfMain.LoadCFG;
var
  cfgname: string;
  cfgf: TJSONConfig;
begin
    // Load settings
  cfgname := GetAppConfigFile(false);
  ForceDirectories(ExtractFileDir(cfgname));
  cfgf := TJSONConfig.Create(nil);
  try
     cfgf.Filename := cfgname;
     cfg.hyper := cfgf.GetValue('/glucose/high', 200);
     cfg.hypo := cfgf.GetValue('/glucose/low', 80);
     cfg.ok := cfgf.GetValue('/glucose/ok', 90);

     cfg.colorval := cfgf.GetValue('/glucose/value', false);
     cfg.colortrend := cfgf.GetValue('/glucose/trend', true);

     cfg.chyper := cfgf.GetValue('/glucose/chigh', clRed);
     cfg.csoonhyper := cfgf.GetValue('/glucose/csoonhigh', clPurple);
     cfg.chypo := cfgf.GetValue('/glucose/clow', clBlue);
     cfg.cok := cfgf.GetValue('/glucose/cok', $0007D121);

     cfg.mmol := cfgf.GetValue('/glucose/mmol', true);
     cfg.url := cfgf.GetValue('/remote/url', '');
     cfg.api := cfgf.GetValue('/remote/key', '');

     cfg.updates := cfgf.GetValue('/remote/freq', 300000);

     cfg.alert := cfgf.GetValue('/dose/alert', false);
     FormStyle := TFormStyle(cfgf.GetValue('/gui/window', ord(fsNormal)));

     cfg.lowexec := cfgf.GetValue('/system/app', '');
     cfg.snooze :=  cfgf.GetValue('/gui/snooze', 30);
     cfg.sndhyper :=  cfgf.GetValue('/audio/high', '');
     cfg.sndhypo :=  cfgf.GetValue('/audio/low', '');

     cfg.arrows := cfgf.GetValue('/gui/arrows', 1);
     cfg.hover := cfgf.GetValue('/gui/hover', false);
     cfg.hovertrans := cfgf.GetValue('/gui/hovertrans', 100);
     cfg.hovercolor := cfgf.GetValue('/gui/hovercolor', false);
     cfg.hoverwindowcolor := cfgf.GetValue('/gui/hoverwindowcolor', false);

     cfg.voice := cfgf.GetValue('/glucose/voice', false);
     cfg.voicetrend := cfgf.GetValue('/glucose/voicetrend', false);
     cfg.voiceall := cfgf.GetValue('/glucose/voiceall', false);

     cfg.mac_dock := cfgf.GetValue('/mac/dock', true);
     cfg.mac_bounce := cfgf.GetValue('/mac/bounce', true);
     cfg.mac_bounce_once := cfgf.GetValue('/mac/bounce_once', true);


     cfgf.free;
  except
   MessageDlg('Error', 'Could not load, or create, the configuration file. Please make sure your AppData folder is writeable.', mtError,
    [mbOK],0);
   Application.Terminate;
   Abort;
  end;

  // Since we initially disable things when no config exists, we need to make sure we enable them now
      btnUpdate.Enabled := true;
      btOS.Enabled := true;

      tUpdate.Interval:=cfg.updates;

end;

// Rotate images, to please the user's preference of direction
procedure MirrorArrow(boxes: array of TImageList; index, dest: integer);
var
  pic, src: TBitmap;
  i, j: integer;
  im: TImageList;
begin
  for im in boxes do begin
    pic := TBitmap.Create;
    src := TBitmap.Create;
    im.GetBitmap(index, src);
    with src do begin
      pic.Width:=im.Width;
      pic.Height:=im.Height;
    for i:=0 to im.Width-1 do
         for j:=0 to im.Height-1 do
           pic.Canvas.Pixels[Width-i-1, j]:=src.Canvas.Pixels[i,j];
     im.Replace(dest, pic, nil);
    end;
    pic.free;
    src.free;
  end;
end;

{$IFDEF LCLCarbon}
procedure TfMain.SetOverlay(trend: HBitmap);
var
  temp_trend: TCarbonBitmap;
  temp_CGImage: CGImageRef;
  temp_CGContext: CGContextRef;
begin
try

    temp_trend := TCarbonBitmap.Create(TCarbonBitmap(trend));
    temp_CGImage := temp_trend.CGImage;
    temp_CGContext := BeginCGContextForApplicationDockTile;
    SetApplicationDockTileImage(temp_CGImage);
   // OverlayApplicationDockTileImage(temp_CGImage);
  //We dont do this when we use a TCarbonBitmap origin  CGImageRelease(temp_CGImage);
    EndCGContextForApplicationDockTile(temp_CGContext);
    temp_trend.Free;
except on E: Exception do
    ShowMessage('An error occured while trying to set the glucose value in the tray' + E.Message);
 end;
end;
{$ENDIF}

procedure TfMain.FormCreate(Sender: TObject);
begin
  // Make sure the splash is showing
  Application.ProcessMessages;

  // macOS integration
  {$IFDEF LCLCarbon}
   NsApp := NSApplication.sharedApplication;
   MenuItem5.Visible := false;
   MenuItem6.Visible := false;
   SetApplicationDockTileMenu(TCarbonMenu(pnMain.handle).menu);
  // NsApp.o
  {$ENDIF}

  // Load settings
  LoadCFG;

  // Check which way the user wants ther arrows facing
  case cfg.arrows of
    1: begin   // Both left
      MirrorArrow([ilBG, ilFull], 5, 5);
    end;
    2: begin  // Both right
      MirrorArrow([ilBG, ilFull], 3, 3);
    end;
  end;

  CheckVesion(ttversion, ttversionpre);
  {$ifdef DARWIN}
    fMain.top := pnTop.Height;

    macmenu := TMainMenu.Create(fMain);
    applemenu := TMenuItem.create(macmenu);
    applemenu.Caption := #$EF#$A3#$BF;  //Unicode Apple logo char
    macmenu.Items.Add(applemenu);
    macabout := TMenuItem.Create(applemenu);
    macabout.Caption := 'About TrayTrend';
    macabout.onclick := @miAboutClickA; // This isnt init'ed right now so we cant just assign miAbout iteself as an item
    applemenu.Add(macabout);
    macsettings := TMenuItem.Create(macmenu);
    macsettings.Caption:=btConf.Caption;
    macsettings.OnClick:=btConf.OnClick;
    applemenu.add(macsettings);
    maccustomization := TMenuItem.Create(fMain);
    maccustomization.Caption:=btOs.Caption;
    maccustomization.OnClick:=btOS.OnClick;
    applemenu.add(maccustomization);

  {$endif}
end;

procedure TfMain.FormShow(Sender: TObject);
begin
{$ifdef Darwin}
  BorderStyle:=bsToolWindow;
{$endif}
  if fHover.Visible then
    fHover.Hide; // We need to trigger "Show" to make the window look right anyways

  // Create the hover window if it's wanted
  if cfg.hover then begin
     fHover.trans := cfg.hovertrans;
     fHover.Visible:=true;
     fHover.lblVal.Caption := FormatBG(bgval, true);
  end;

  // Check if we have any useable settings data
  if cfg.url <> '' then
      UpdateBG
  else begin
      // Disable the GUI elements if we have no data
      btnUpdate.Enabled := false;
      btOS.Enabled := false;
  end;
end;

// Minimizing the main window also minimizes the hover window, so we need to prevent this
procedure TfMain.FormWindowStateChange(Sender: TObject);
begin
  if (assigned(fhover)) and (WindowState = wsMinimized) then begin
     WindowState := wsNormal;
     Hide;
     ttMsg('Double-click the floating window to show TrayTrend again!');
  end;
end;

// Show ther main form
procedure TfMain.MenuItem1Click(Sender: TObject);
begin
  Show;
  WindowState := wsNormal;
  ShowInTaskBar := stDefault;
  BringToFront;
end;

procedure TfMain.MenuItem3Click(Sender: TObject);
begin
  btConf.Click;  // To avid redundancy we just trigger "click" on a button that does what we want already
end;

procedure TfMain.MenuItem4Click(Sender: TObject);
begin
  btOS.Click;
end;

procedure TfMain.MenuItem5Click(Sender: TObject);
begin
  OpenURL(cfg.url); // Open Nightscout in the user's browser of choise
end;

procedure TfMain.miAboutClickA(Sender: TObject);
begin
  ttMsg('TrayTrend is a desktop monitor for the NightScout system, licensed under the GNU Public License v3. Original work by Björn Lindh: github.com/slicke');
end;

procedure TfMain.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfMain.tTrayClick(Sender: TObject);
begin

end;

// Update the readings when needed
procedure TfMain.tUpdateTimer(Sender: TObject);
begin
UpdateBG;
end;

procedure TfMain.btnUpdateClick(Sender: TObject);
begin
  UpdateBG;
end;

// Open up the settings box and set the current values
procedure TfMain.btConfClick(Sender: TObject);
begin
  fSettings.edSecret.Text := cfg.api;
  fSettings.edURL.Text := cfg.url;
  fSettings.rbMmol.Checked := cfg.mmol;
  fSettings.fnHigh.FileName := cfg.sndhyper;
  fSettings.fnLow.FileName := cfg.sndhypo;
  fSettings.seFreq.Value := round(cfg.updates/60000);
  fSettings.cbVoice.Checked := cfg.voice;
  fSettings.cbVoiceAll.Checked := cfg.voiceall;
  fSettings.cbVoiceTrend.Checked := cfg.voicetrend;

  fSettings.ShowModal;
  // Modal pauses until the window closes. When it closes, it rewrites the config file and then we load it again
  tUpdate.Interval:=cfg.updates;
  LoadCFG;
  btnUpdate.Click;
end;

// Open up the non-NS settings box
procedure TfMain.btOSClick(Sender: TObject);
begin
  fSysSettings.pnOK.Color := cfg.cok;
  fSysSettings.pnLow.Color := cfg.chypo;
  fSysSettings.pnSoonHigh.Color := cfg.csoonhyper;
  fSysSettings.pnHigh.Color := cfg.chyper;
  fSysSettings.cbOnTop.Checked := (self.FormStyle = fsSystemStayOnTop);
  fSysSettings.tbSnooze.Position :=  cfg.snooze;
  fSysSettings.lblSnooze.Caption := 'Snooze time: ' + IntToStr(cfg.snooze) + ' minutes';
  fSysSettings.cbValue.Checked :=  cfg.colorval;
  fSysSettings.cbTrend.Checked := cfg.colortrend;
  fSysSettings.cbrun.Checked := cfg.lowexec <> '';
  fSysSettings.fnrun.Enabled := cfg.lowexec <> '';
  fSysSettings.fnRun.FileName:= cfg.lowexec;
  fSysSettings.cbHover.Checked := cfg.hover;
  fSysSettings.seHover.Value := cfg.hovertrans;
  fSysSettings.cbHoverColor.Checked := cfg.hovercolor;
  fSysSettings.cbHoverWindowColor.Checked := cfg.hoverwindowcolor;

  if cfg.mmol then begin
    fSysSettings.seHigh.DecimalPlaces:=2;
    fSysSettings.selOW.DecimalPlaces:=2;
    fSysSettings.seok.DecimalPlaces:=2;
    fSysSettings.seHigh.value:= ConvertBGUnit(cfg.hyper, false);
    fSysSettings.selOW.value:=ConvertBGUnit(cfg.hypo, false);
    fSysSettings.seok.value:=ConvertBGUnit(cfg.ok, false);
  end else begin
    fSysSettings.seHigh.DecimalPlaces:=0;
    fSysSettings.selOW.DecimalPlaces:=0;
    fSysSettings.seok.DecimalPlaces:=0;
    fSysSettings.seHigh.value:=cfg.hyper;
    fSysSettings.selOW.value:=cfg.hypo;
    fSysSettings.seok.value:=cfg.ok;
  end;


  if cfg.arrows = 1 then
     fSysSettings.cbArrowRight.Checked:=true
  else if cfg.arrows = 2 then
      fSysSettings.cbArrowLeft.Checked:=true
  else
      fSysSettings.cbArrowMix.Checked:=true;

  // cfg.mac_dock;
   fSysSettings.cbBouce.Checked := cfg.mac_bounce;
   fSysSettings.cbBounceOne.Checked := cfg.mac_bounce_once;


  fSysSettings.ShowModal;
  // Since Modal is blocking, the form will write a new cfg which we then load
  // We need to reset these if the color is disabled
  lblTrend.Font.Color:=clDefault;
  lblVal.Font.Color:=clDefault;

  LoadCFG;
  UpdateBG;
  FormShow(self);
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Visible then begin
    ttAlert('TrayTrend has been minimized, but is still running. Click the tray icon to restore.', imTrend.Picture.Bitmap);
    WindowState := wsNormal;
    Hide;
    ShowInTaskBar := stNever;
    CloseAction := caNone;// caHide;
  end else begin
    CloseAction := caFree;
    Application.Terminate;
  end;
end;

// Get a good readable color based on the background
function GetHoverColor(const AColor: TColor): TColor;
var
  R, G, B: single;
begin
  R := GetRValue(AColor) * 0.25;
  G := GetGValue(AColor) * 0.625;
  B := GetBValue(AColor) * 0.125;

  if (R + G + B) > 128 then begin
    result := clBlack;
  end else begin
    result := clWhite;
  end;
end;

// Continuation of GetHoverColor, though a bit shaded
function GetHoverTrendColor(const AColor: Tcolor): TColor;
begin
  if GetHoverColor(AColor) = clWhite then
      result := $00F2F2F2
  else
      result := $00484848;
end;

// Paint the graphical things
function TfMain.SetUI(bgval: single; title: string; lbl: tlabel; img: tosimg; smallimg: ticon): tcolor;
var
  i: integer;
  snoozed: int64;
  tr: TBGTrend;
  {$ifdef Windows}
    voice: OLEVariant;
    SavedCW: Word;
    speech: widestring;
  {$endif}
  {$ifdef Darwin}
    speech: string;
    ss: TSpeechSynthesizer;
  {$endif}
begin
  // Parse the trend and handle none
  if title= '' then begin
    tr := NO_DATA;
    lblVal.Font.Color:=clNone;
    lbl.Caption := GetTrendName(tr);
    //imTrend.Picture.Clear;
    if cfg.voice or cfg.voiceall then begin
      {$ifdef windows}
      try
      voice := CreateOLEObject('SAPI.SpVoice');
      if cfg.voice then
          voice.Speak('TrayTrend has not recieved any glucose reading', 0);
      voice := Unassigned;
      finally
        voice := Unassigned;
      end;
      {$endif}
      {$ifdef darwin }
      try
        ss := TSpeechSynthesizer.Create;
        ss.StartSpeakingString('TrayTrend has not recieved any glucose reading');
      finally
        ss.Free;
      end;
      {$endif}
    end;
    Exit;
  end;

  try
    ReadStr(title, tr) // Parse the trend
  except
    tr := NOT_COMPUTABLE;
  end;

  lastbgtrend := tr;  // Since we're setting a new trend, store the "current" one as the "last" one

  // Calculate snooze time
  snoozed := MinutesBetween(Now, lastalertts);
  // Set the "user firendly" trend name
  lbl.Caption := GetTrendName(tr);
  // Assign the right icon and text color
  i := ord(tr);

  // Fix GUI things
  result := GetBGColor(bgval);

  // Only handle the hover window if it's assigned/created
  if assigned(fHover) then
    fHover.lblTrend.Font.Color := $00F2F2F2;
  if cfg.colortrend then
     lbl.Font.Color := result;
  if cfg.colorval then
     lblVal.Font.Color := result;
  if (cfg.hoverwindowcolor) and assigned(fHover) then begin
     fHover.Color := result;

    // Set the text colors so they're visible
    fHover.lblVal.Font.Color := GetHoverColor(result);
    fHover.lblTrend.Font.Color := GetHoverTrendColor(result);

    end else if (cfg.hovercolor) and assigned(fHover) then begin
    // If we're not coloring the window, ust use defaults
     fHover.lblVal.Font.Color := result;
     fHover.Color:=clBlack;
    end;

    // Set icons
    ilBG.GetIcon(i, smallimg);
    {$ifdef OSBitmap}
      ilFull.GetBitmap(i, img);
    {$else}
      ilFull.GetIcon(i, img);
    {$endif}

    // Manage notifications
    if (bgval > cfg.hyper) or (bgval < cfg.hypo) then begin
    {$ifdef Windows}
      if (bgval > cfg.hyper) and (cfg.sndhyper <> '') then
        sndPlaySound(pchar(cfg.sndhyper), snd_Async or snd_NoDefault)
      else if (bgval < cfg.hypo) and (cfg.sndhypo <> '') then
        sndPlaySound(pchar(cfg.sndhypo), snd_Async or snd_NoDefault);
    {$endif} // Fix macOS/linux later

        // Do text-to-speech strings and talk
        if cfg.voice then begin

        if bgval > cfg.hyper then
          speech := 'High blood glucose. '+ FormatBG(bgval, true)+'!'
        else if bgval < cfg.hypo then
          speech := 'Low blood glucose. '+ FormatBG(bgval, true)+'!'
        else if cfg.voiceall then
          speech := 'Blood glucose is '+ FormatBG(bgval, true)+'!';


        if (bgtrend <> 'Steady') and (cfg.voicetrend) then
           speech := speech+' Glucose trend is '+ lbl.Caption+'.';
        {$ifdef Windows}
        try
          // Change FPU interrupt mask to avoid SIGFPE exceptions
          SavedCW := Get8087CW;
          voice := CreateOLEObject('SAPI.SpVoice');
          Set8087CW(SavedCW or $4);
          if speech <> '' then begin
            voice.Speak('TrayTrend Update! '+speech+ ' Reading uploaded '+ StringReplace(lblTimeAgo.Caption, '(s)', 's',[]), 1);
          end;
        finally
          // Restore FPU mask
          Set8087CW(SavedCW);
          voice:=Unassigned;
        end;
    {$endif}
    {$ifdef darwin }
    try
      ss := TSpeechSynthesizer.Create('Ava');
      if speech <> '' then
         ss.StartSpeakingString('TrayTrend Update! '+speech+ ' Reading uploaded '+ StringReplace(lblTimeAgo.Caption, '(s)', 's',[]));
    finally
      ss.Free;
    end;
    {$endif}


    // Show an alert if not snoozed
    if snoozed >= cfg.snooze then begin
     // ilFull.GetIcon(i, notifi.Icon.Icon);
      ttAlert(lbl.Caption+' - '+lblTimeAgo.caption+LineEnding+LineEnding+'Current value: ' + FormatBG(bgval, false)+LineEnding+'Last value: '+FormatBG(lastbg, false), imTrend.Picture.Bitmap);
      //notifi.Show;
      lastalert := tr;
      lastalertts := Now;
      {$IFDEF LCLCarbon}
      if cfg.mac_bounce then
        NSApp.requestUserAttention(NSCriticalRequest);
      {$ENDIF}

      lblSnooze.Caption := '[Alerts snoozed for '+inttostr(cfg.snooze)+ ' min]';
    end else if (snoozed < cfg.snooze) then // Add a note that we're snoozing
          lblSnooze.Caption := '[Alerts snoozed for '+ inttostr(cfg.snooze-snoozed)+' min]';
    end else begin
    // If we're not high or low, we can clear any alerts
      lastalert := NONE;
      lastalertts := Now;
      lblSnooze.Caption := '';
      {$IFDEF LCLCarbon}
      if cfg.mac_bounce_once then
         NSApp.requestUserAttention(NSInformationalRequest);
      {$ENDIF}
    end;
  end;

end;

// Get the correct color for a BG value in the UI
function TfMain.GetBGColor(val: single): tcolor;
begin
  if val > cfg.hyper then
      result := cfg.chyper
  else if val < cfg.hypo then
      result := cfg.chypo
  else if 1.25 >= cfg.hyper/val then
      result := cfg.csoonhyper
  else
      result := cfg.cok;
end;

// Get an UTF char representing a trend arrow
function GetUTFArrow(trend: TBGTrend): UTF8String;
begin
case trend of
  Flat: result := '→';
  DoubleDown: result := '↓↓';
  DoubleUp: result := '↑↑';
  FortyFiveDown: result := '⭝';
  FortyFiveUp: result := '⭜';
  SingleDown: result :=  '↓';
  SingleUp: result := '↑';
  else
    result := 'ERR';
end;
end;

// Update the readings. A big part of the tray icon code is code based on FPC documentation for generating icons on-the-go
procedure TfMain.UpdateBG;
var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  w, h: Integer;
  TempBitmap{$ifdef Darwin}, macbitmap{$endif}: TBitmap;
  bgarrow: ticon;
  bgcolor: tcolor;
  bgreading: string;
begin
  try
    FetchValues;
  except
    ttMsg('Error contacting NightScout');
    Exit;
  end;

  w := 24;
  h := 24;
  try
    TempIntfImg := TLazIntfImage.Create(w, h);
    TempBitmap := TBitmap.Create;
    TempBitMap.Masked:=true;
    TempBitMap.SetSize(w, h);
    TempBitMap.Canvas.Brush.Style:=bsSolid;
    bgarrow := tIcon.Create;
    {$ifdef Darwin}
    bgcolor := SetUI(bgval, bgtrend, lbltrend, imTrend.Picture.Bitmap, bgarrow);
    {$else}
    bgcolor := SetUI(bgval, bgtrend, lbltrend, imTrend.Picture.Icon, bgarrow);
    {$endif}
    TempBitMap.Canvas.Brush.Color := bgcolor;
    TempBitMap.Canvas.FillRect(0, 0, w, h);
    TempBitMap.Canvas.Font:=Canvas.Font;
//    TempBitMap.Canvas.Draw(0, 0, bgarrow);

    TempBitmap.Canvas.Font.Color := GetHoverColor(bgcolor);
    {$ifdef windows}
      TempBitmap.Canvas.Font.Name := 'Trebuchet MS';
      TempBitmap.Canvas.Font.Style := [fsBold];
      TempBitmap.Canvas.Font.Quality:=fqCleartype;
      TempBitmap.Canvas.Font.Size := 9;
    {$endif}
    if cfg.mmol then begin
      TempBitmap.Canvas.Font.Size := 8;
      TempBitMap.Canvas.TextOut(0, 0 , inttostr(trunc(ConvertBGUnit(bgval, false))));//0,0,'10.2');
      TempBitMap.Canvas.TextOut(TempBitmap.width-tempBitmap.Canvas.Font.Size-1, 7 , inttostr(round(frac(ConvertBGUnit(bgval, false))*10)));
    end else
      TempBitMap.Canvas.TextOut(0, 0 , FormatBG(bgval, true));//0,0,'10.2');
//    TempBitMap.Canvas.TextOut(0,10,GetUTFArrow(lastbgtrend));//0,0,'10.2');
    miTrend.ImageIndex := ord(lastbgtrend);
    imTrend.Caption := FormatBG(bgval, true) + lblTrend.Caption;
     {$ifdef DARWIN}
     miTrend.Caption := imTrend.Caption;
     {$endif}

    TempIntfImg.LoadFromBitmap(TempBitmap.Handle, TempBitmap.MaskHandle);


    TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle, False);
    TempBitmap.Handle := ImgHandle;
    TempBitmap.MaskHandle := ImgMaskHandle;

    tTray.Icon.Assign(TempBitmap);
    tTray.Show;
    {$ifdef Darwin}
    try
      macBitmap := TBitmap.Create;
      macBitmap.Width := imMacDock.Picture.Bitmap.Width;
      macBitmap.Height := imMacDock.Picture.Bitmap.Height;
     // macBitmap.TransparentMode := tmFixed;
   //   macbitmap.TransparentColor:=clWhite;
    //  macbitmap.Transparent:=true;


      macbitmap.Canvas.brush.Style := bsClear;
     // macbitmap.Canvas.FillRect(0, 0, macbitmap.Width, macbitmap.Height) ;
      macBitmap.Canvas.Draw(0,0, imMacDock.Picture.Bitmap);

      bgreading := FormatBG(bgval, true);

      macBitmap.Canvas.Font.Color := bgcolor;
      macbitmap.Canvas.Font.size := 100;
      macbitmap.Canvas.Font.Name := 'Verdana';
      macbitmap.Canvas.Font.Quality := fqAntialiased;
      macBitMap.Canvas.TextOut(round((macbitmap.Canvas.width-2-macbitmap.canvas.TextWidth(bgreading))/2), 1 , bgreading);


      if cfg.mac_dock then
        SetOverlay(macBitmap.Handle);
    except on e: exception do begin
      cfg.mac_dock:=false;
      ttmsg('Glucose level will not show on the dock, due to an error: ' + E.Message);
    end;
    end;

    {$endif}


    if assigned(fHover) then begin
      fHover.lblVal.Caption := FormatBG(bgval, true);
      fHover.lblTrend.Caption := lblTrend.Caption;
    end;

  finally
    TempIntfImg.Free;
    TempBitmap.Free;
    {$ifdef Darwin}
      macbitmap.free;
    {$endif}
  end;
  lblVal.caption := FormatBG(bgval, false);
  lblTrend.Width := lblVal.Width;
end;

{$R *.lfm}

end.

