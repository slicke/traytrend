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
unit uconfig;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, StdCtrls,
  EditBtn, Spin, jsonConf, extctrls, buttons, Classes,
  fphttpclient, sha1, StrUtils, stuff;

type

  { TfSettings }

  TfSettings = class(TForm)
    btnOK: TBitBtn;
    btnOK1: TBitBtn;
    btnOK2: TBitBtn;
    btnCheck: TBitBtn;
    cbVoice: TCheckBox;
    cbVoiceTrend: TCheckBox;
    cbVoiceAll: TCheckBox;
    edURL: TEdit;
    edSecret: TEdit;
    fnLow: TFileNameEdit;
    fnHigh: TFileNameEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    rbMmol: TRadioButton;
    rbMgdl: TRadioButton;
    seFreq: TSpinEdit;
    procedure btnOK1Click(Sender: TObject);
    procedure btnCheckChangeBounds(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnOK3DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  fSettings: TfSettings;

implementation

{$R *.lfm}

{ TfSettings }

// Save choises when OK is clicked
procedure TfSettings.btnOKClick(Sender: TObject);
var
  c: TJSONConfig;
  cfgname: string;
begin
  cfgname := GetAppConfigFile(false);
  ForceDirectories(ExtractFileDir(cfgname));
  c := TJSONConfig.Create(nil);

  try
     c.Filename := cfgname;
     c.SetValue('/glucose/mmol', rbMmol.Checked);
     c.SetValue('/remote/url', edURL.Text);
     c.SetValue('/remote/key', edSecret.Text);
     c.SetValue('/remote/freq', seFreq.Value*60000);
     c.SetValue('/audio/low', fnLow.FileName);
     c.SetValue('/audio/high', fnHigh.FileName);

     c.SetValue('/glucose/voice', cbVoice.Checked);
     c.SetValue('/glucose/voicetrend', cbVoiceTrend.Checked);
     c.SetValue('/glucose/voiceall', cbVoiceAll.Checked);

     c.SetValue('/remote/freq', seFreq.Value*60000);
  except
   MessageDlg('Error', 'Could not load, or create, the configuration file. Please make sure your AppData folder is writeable.', mtError,
    [mbOK],0);
   Application.Terminate;
   Abort;
  end;
  c.free;
  Close;
end;

procedure TfSettings.FormShow(Sender: TObject);
begin

end;

procedure TfSettings.btnOK1Click(Sender: TObject);
begin
  Close;
end;

procedure TfSettings.btnCheckChangeBounds(Sender: TObject);
begin

end;

procedure TfSettings.btnCheckClick(Sender: TObject);
var
  ans: string;
  code: integer;
begin
  code := -1;
  try
   with TFPHTTPClient.Create(nil) do
   try
     AddHeader('API-SECRET', SHA1Print(SHA1String(edSecret.Text)));
     ans := Get(edURL.text + '/api/v1/status.json');
   finally
     code := ResponseStatusCode;
     Free;
   end;
   except

   end;
   if code = 404 then
     ttMsg('No page was found at that address')
   else if code = 401 then
     Application.MessageBox('Your API key is wrong', 'Authentication failure')
   else if code = 200 then
     stuff.ttMsg('NightScout connection was successful', 'Success', 'Great!')
   else
     Application.MessageBox(PChar(ans), 'Unknown response');
   stuff.ttMsg('Testing' ,'This is a long message! A default parameter also referred to as optional argument (or default argument) is a function or procedure parameter that has a default value provided to it. If the programmer does not supply a value for this parameter, the default value will be used. If the programmer does supply a value for the default parameter, the programmer-supplied value is used.',100);

end;

procedure TfSettings.btnOK3DragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TfSettings.Image1Click(Sender: TObject);
begin

end;

end.

