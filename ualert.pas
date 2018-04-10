unit ualert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, lclintf, StdCtrls;

type

  { TfAlert }

  TfAlert = class(TForm)
    imTrend: TImage;
    lblMsg: TLabel;
    pnTrend: TPanel;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

  public

  end;

var
  fAlert: TfAlert;

implementation
uses stuff;

{$R *.lfm}

{ TfAlert }

procedure TfAlert.FormCreate(Sender: TObject);
var
  ABitmap: TBitmap;
begin
  {$ifdef Windows}
(*  BorderStyle:=bsNone;
  ABitmap := TBitmap.Create;
  ABitmap.Monochrome := True;
  ABitmap.Width := Width; // or Form1.Width
  ABitmap.Height := Height; // or Form1.Height

  ABitmap.Canvas.Brush.Color:=clBlack;
  ABitmap.Canvas.FillRect(0, 0, Width, Height);

  ABitmap.Canvas.Brush.Color:=clWhite;
  ABitmap.Canvas.RoundRect(0, 0, Width, Height, 50, 50);

    Canvas.Draw(0,0,ABitmap);
    SetShape(ABitmap);
     ABitmap.Free;     *)
  {$endif}
  {$ifndef XWindows}
  color := clblack;
  BorderStyle:=bsNone;
  lblmsg.Font.color := clwhite;
  {$endif}
  {$if Defined(Darwin)}
  left := screen.Width-width-10;
  top := 25;
  {$elseif Defined(Windows)}
  left := screen.Width-width-10;
  top := screen.Height-25;
  {$else}
  left := 10;
  top := 25;
  {$endif}

end;

procedure TfAlert.FormDestroy(Sender: TObject);
begin

end;

procedure TfAlert.FormShow(Sender: TObject);
begin
  self.AlphaBlendValue := 200;
  self.AlphaBlend := True;
end;

procedure TfAlert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction := caFree;
end;

procedure TfAlert.FormClick(Sender: TObject);
begin
  Hide;
  Close;
  stuff.fixAlertPos;
end;



end.

