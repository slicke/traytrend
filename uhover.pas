unit uhover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfHover }

  TfHover = class(TForm)
    lblTrend: TLabel;
    lblVal: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure lblTrendClick(Sender: TObject);
    procedure lblValClick(Sender: TObject);
    procedure lblValMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblValMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure lblValMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public
    trans: integer;

  end;

var
  fHover: TfHover;
  mdown: boolean;
  PX, PY: integer;

implementation

{$R *.lfm}

{ TfHover }

procedure TfHover.FormShow(Sender: TObject);
begin
  self.AlphaBlendValue := trans; // 0..255
  self.AlphaBlend := True;

end;

procedure TfHover.lblTrendClick(Sender: TObject);
begin

end;

procedure TfHover.lblValClick(Sender: TObject);
begin

end;

procedure TfHover.lblValMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    mdown := True;
    PX := X;
    PY := Y;
  end;// else if Button = mbRight then
//    self.PopupMenu.PopUp;
end;

procedure TfHover.lblValMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if mdown then begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;
end;

procedure TfHover.lblValMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdown :=False;
end;

procedure TfHover.FormMouseEnter(Sender: TObject);
begin
  self.AlphaBlendValue := 240;
end;

procedure TfHover.FormCreate(Sender: TObject);
begin
    left := screen.Width-width;
    top := screen.height-height;
end;

procedure TfHover.FormDblClick(Sender: TObject);
begin
  lblVal.PopupMenu.Items[0].Click;
end;

procedure TfHover.FormMouseLeave(Sender: TObject);
begin
    self.AlphaBlendValue := trans;
end;

procedure TfHover.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

end.

