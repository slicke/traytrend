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
unit uhover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TfHover }

  TfHover = class(TForm)
    lblTrend: TLabel;
    lblVal: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MoveMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MoveMouse(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure MoveMouseUp(Sender: TObject; Button: TMouseButton;
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

// Set transparency when window shows
procedure TfHover.FormShow(Sender: TObject);
begin
  self.AlphaBlendValue := trans; // 0..255
  self.AlphaBlend := True;
end;

// Allow window movement by dragging, detect when form is clicked
procedure TfHover.MoveMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    mdown := True;
    PX := X;
    PY := Y;
  end;
end;

// Allow window movement by dragging, if user has clicked the form and is dragging - move
procedure TfHover.MoveMouse(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if mdown then begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;
end;

// Allow window movement by dragging, stop movement when button is released
procedure TfHover.MoveMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdown :=False;
end;

// Make the form more visible when the mouse is over it
procedure TfHover.FormMouseEnter(Sender: TObject);
begin
  self.AlphaBlendValue := 240;
end;

// Position the window in the lower right when it's created
procedure TfHover.FormCreate(Sender: TObject);
begin
    left := screen.Width-width;
    top := screen.height-height;
end;

// Show the main form by simulating clicking the tray pop-up menu, when form is double clicked
procedure TfHover.FormDblClick(Sender: TObject);
begin
  lblVal.PopupMenu.Items[0].Click;
end;

// Restore transparency when mouse leaves form
procedure TfHover.FormMouseLeave(Sender: TObject);
begin
    self.AlphaBlendValue := trans;
end;

end.

