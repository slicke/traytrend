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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  intfgraphics, lazcanvas, LCLType, StdCtrls, EditBtn, Buttons, PopupNotifier,
  fpImage, math, fphttpclient, sha1, fpjson, jsonparser, dateutils, jsonconf,
  lazutf8sysutils, uconfig, typinfo, usys;

type

  // Settings the user has
  TUserVals = record
    ok, hypo, hyper: single;
    cok, chypo, chyper, csoonhyper: tcolor;
    mmol: boolean;
    url, api, lowexec: string;
    alert, colorval, colortrend: boolean;
    snooze: integer;
  end;

  // Ported from server source code
  TDirection = (NONE, DoubleUp, SingleUp, FortyFiveUp, Flat, FortyFiveDown, SingleDown, DoubleDown, NOT_COMPUTABLE, RATE_OUT_OF_RANGE);

  { TfMain }

  TfMain = class(TForm)
    btnUpdate: TBitBtn;
    btConf: TButton;
    btOS: TButton;
    ilBG: TImageList;
    ilFull: TImageList;
    imTrend: TImage;
    Label1: TLabel;
    lblSnooze: TLabel;
    lblSpeed: TLabel;
    lblTimeAgo: TLabel;
    Label5: TLabel;
    lblTrend: TLabel;
    lblVal: TLabel;
    pnTop: TPanel;
    pnAlert: TPopupNotifier;
    tUpdate: TTimer;
    tTray: TTrayIcon;
    procedure btnUpdateClick(Sender: TObject);
    procedure btConfClick(Sender: TObject);
    procedure btOSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tUpdateTimer(Sender: TObject);
    procedure updateTrend(velocity: single; desc, device: string; newdate: tdatetime);
  private
    procedure fetchValues;
  public
    procedure UpdateBG;
    function SetUI(bgval: single; title: string; lbl: tlabel; img, smallimg: ticon; notifi: TPopupNotifier): tcolor;
    function formatBG(val: single; short: boolean): string;
    function convertBG(val: single; ismmol: boolean): single;
    procedure LoadCFG;
    function GetBGColor(val: single): tcolor;
    function GetDirectionName(dir: TDirection): string;
  end;

var
  fMain: TfMain;
  cfg: TUserVals;
  lastbg: single = -1;
  bgval: single;
  lastread: int64;
  bgtrend: string;
  lastalert: TDirection = NONE;
  lastalertts: TDateTime;

implementation

// Process a trend/direction to a GUI string
function tfMain.GetDirectionName(dir: TDirection): string;
begin
  case dir of
    NONE:
      result := 'No direction';
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
  end;
end;

// Fetch a JSON resource form Nightscout
function DoNSReq(metric: string): TJSONData;
var
  ans : string;
begin
   with TFPHTTPClient.Create(nil) do
   try
     AddHeader('API-SECRET', SHA1Print(SHA1String(cfg.api)));
     ans := Get(cfg.url + '/api/v1/'+metric+'.json');
     result := GetJSON(ans);
   finally
     Free;
   end;
end;

// Get glucose values from NS
procedure tfMain.fetchValues;
var
  val, res: TJSONData;
  i: integer;
  ts: int64;
  tdate: TDateTime;
begin
  // Contact the API over SSL
  lblTimeAgo.Caption:= 'Updating now';
  Application.ProcessMessages;
  res := DoNSREq('entries');

  // Go through all resturned values in reverse order, so we enter them in chronological order
  for i := res.Count-1 downto 0 do begin
    val := res.Items[i];
    lastbg := bgval;
    bgval := val.findpath('sgv').AsInteger;
    ts := val.findpath('date').AsInt64;

    // We dont have to do things twice
    if ts = lastread then
        Exit;
    lastread := ts;

    // We get milliseconds here, wehich we remove
    tdate := UnixToDateTime(round(ts/1000));
    updateTrend(val.findpath('delta').AsInteger, val.findpath('direction').AsString, val.findpath('device').AsString, tdate);
  end;
end;

// Process a new value/reading and put it in the GUI
procedure TfMain.updateTrend(velocity: single; desc, device: string; newdate: TDateTime);
var
  datediff: int64;
begin
  // Update labels with basic info
  bgtrend := desc;
  lblSpeed.Caption := formatbg(velocity, false);
  if velocity > 0 then
    lblSpeed.Caption := '+'+lblSpeed.Caption;

   // Figure out how to explain the time diff
  datediff := MinutesBetween(NowUTC, newdate);

  // We dont want to display 120 minutes, 2 hours is better
  if datediff <= 60 then
    lblTimeAgo.Caption := Format('%d minute(s) ago', [datediff])
  else if datediff >= 1440 then
    lblTimeAgo.Caption := Format('%d day(s) ago', [DaysBetween(NowUTC, newdate)])
  else
    lblTimeAgo.Caption := Format('%d hour(s) ago', [HoursBetween(NowUTC, newdate)]);

  // We set the full time as a hint if one hovers above the time
  lblTimeAgo.Hint := DateTimeToStr(newdate);

  // If the user wants to run a process if low, we do it here
  if (bgval < 3) and (cfg.lowexec <> '') and (MinutesBetween(Now, lastalertts) >= cfg.snooze) then
    ExecuteProcess(Utf8ToAnsi(cfg.lowexec), '', []);
end;

// Process a reading into a good value.
function TfMain.formatBG(val: single; short: boolean): string;
begin
  if short then begin  // Short determines if we add mmol/mg/dl at the end
      if cfg.mmol then
          result := FloatToStrF(convertBG(val, false), ffFixed, 3, 1) // We get mg/dl from NS so we need to convert
      else
          result := FloatToStrF(val, ffNumber, 3, 0);
  end
  else if cfg.mmol then
    result := Format('%s mmol/L', [FloatToStrF(convertBG(val, false), ffFixed, 3, 1)])
  else
    result := Format('%s mg/dL', [FloatToStrF(val, ffNumber, 3, 0)])
end;

// Convert units between one and other
function TfMain.convertBG(val: single; ismmol: boolean): single;
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

     cfg.alert := cfgf.GetValue('/dose/alert', false);
     FormStyle := TFormStyle(cfgf.GetValue('/gui/window', ord(fsNormal)));

     cfg.lowexec := cfgf.GetValue('/system/app', '');
     cfg.snooze :=  cfgf.GetValue('/gui/snooze', 30);;

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
end;


procedure TfMain.FormCreate(Sender: TObject);
begin
  // Make sure the splash is showing
  Application.ProcessMessages;
  // Load settings
  LoadCFG;

  // Check if we have any useable settings data
  if cfg.url <> '' then
      UpdateBG
  else begin
      // Disable the GUI elements if we have no data
      btnUpdate.Enabled := false;
      btOS.Enabled := false;
  end;
end;

// Update the readings when needed
procedure TfMain.tUpdateTimer(Sender: TObject);
begin
  UpdateBG;
end;

procedure TfMain.btnUpdateClick(Sender: TObject);
begin
  updatebg;
end;

// Open up the settings box
procedure TfMain.btConfClick(Sender: TObject);
begin
  fSettings.edSecret.Text := cfg.api;
  fSettings.edURL.Text := cfg.url;
  fSettings.rbMmol.Checked := cfg.mmol;
  fSettings.ShowModal;
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
  fSysSettings.cbAlert.Checked := cfg.alert;
  fSysSettings.cbOnTop.Checked := (self.FormStyle = fsSystemStayOnTop);
  fSysSettings.tbSnooze.Position :=  cfg.snooze;
  fSysSettings.lblSnooze.Caption := 'Snooze time: ' + IntToStr(cfg.snooze) + ' minutes';
  fSysSettings.cbValue.Checked :=  cfg.colorval;
  fSysSettings.cbTrend.Checked := cfg.colortrend;
  fSysSettings.cbrun.Checked := cfg.lowexec <> '';
  fSysSettings.fnrun.Enabled := cfg.lowexec <> '';
  fSysSettings.fnRun.FileName:= cfg.lowexec;

  fSysSettings.ShowModal;
  // We need to reset these if the color is disabled
  lblTrend.Font.Color:=clDefault;
  lblVal.Font.Color:=clDefault;
  LoadCFG;
  UpdateBG;
end;

// Figure out which image to show when the trend changes and the name
function TfMain.SetUI(bgval: single; title: string; lbl: tlabel; img, smallimg: ticon; notifi: TPopupNotifier): tcolor;
var
  i: integer;
  snoozed: int64;
  dir: TDirection;
begin
  // Parse the direction
  try
    ReadStr(title, dir);
  except
    dir := NOT_COMPUTABLE;
  end;

  // Calculate snooze time
  snoozed := MinutesBetween(Now, lastalertts);
  // Set the "user firendly" direction name
  lbl.Caption := GetDirectionName(dir);
  // Assign the right icon and text color
  i := ord(dir);
  // Fix GUI things
  result := GetBGColor(bgval);
  if cfg.colortrend then
     lbl.Font.Color := result;
  if cfg.colorval then
     lblVal.Font.Color := result;
  ilBG.GetIcon(i, smallimg);
  ilFull.GetIcon(i, img);


  // Manage notifications
  if (bgval > cfg.hyper) or (bgval < cfg.hypo) then begin
    if (assigned(notifi)) (*and (lastalert <> dir)*) and (snoozed >= cfg.snooze) then begin
      ilFull.GetIcon(i, notifi.Icon.Icon);
      notifi.Text := lbl.Caption+' - '+lblTimeAgo.caption+LineEnding+LineEnding+'Current value: ' + formatBG(bgval, false)+LineEnding+'Last value: '+formatBG(lastbg, false);
      notifi.Show;
      lastalert := dir;
      lastalertts := Now;

      lblSnooze.Caption := '(snoozing next alert for '+inttostr(cfg.snooze)+ ' minutes)';
    end else if (snoozed < cfg.snooze) then // Add a note that we're snoozing
          lblSnooze.Caption := '(alert snoozed '+ inttostr(cfg.snooze-snoozed)+' minutes)';
  end else begin
    lastalert := NONE;
    lastalertts := Now;
    lblSnooze.Caption := '';
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

// Update icons. A big part is code based on FPC documentation for generating icons on-the.go
procedure TfMain.UpdateBG;
  var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  px, py, w, h: Integer;
  TempBitmap: TBitmap;
  bgarrow: ticon;
  bgcolor: tcolor;
begin
  try
    fetchValues;
  except
    ShowMessage('Error contacting NightScout');
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
    bgcolor := SetUI(bgval, bgtrend, lbltrend, imTrend.Picture.Icon, bgarrow, pnAlert);
    TempBitMap.Canvas.Brush.Color := bgcolor;
    TempBitMap.Canvas.FillRect(0, 0, w, h);
    TempBitMap.Canvas.Font:=Canvas.Font;
    TempBitMap.Canvas.Draw(0, 0, bgarrow);

    TempBitMap.Canvas.TextOut(0,0,formatBG(bgval, true));//0,0,'10.2');
    TempIntfImg.LoadFromBitmap(TempBitmap.Handle, TempBitmap.MaskHandle);


    TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle, False);
    TempBitmap.Handle := ImgHandle;
    TempBitmap.MaskHandle := ImgMaskHandle;

    tTray.Icon.Assign(TempBitmap);
    tTray.Show;

  finally
    TempIntfImg.Free;
    TempBitmap.Free;
  end;
  lblVal.caption := formatBG(bgval, false);
end;

{$R *.lfm}

end.

