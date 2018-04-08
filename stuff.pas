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
unit stuff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uInfo, uAlert, graphics, LCLType, lclintf;


function ttMsg(title, msg: string; btntxt: string = 'OK'; modal: boolean = true): boolean;
function ttMsg(title, msg: string; heightadd: integer; btntxt: string = 'OK'; modal: boolean = true): boolean;

function ttMsg(msg: string): boolean;

procedure ttAlert(msg: string; pic: tbitmap; modal: boolean = false);
procedure fixAlertPos;

implementation
var
  activewins: TList;

function ttMsg(msg: string): boolean;
begin
  ttMsg('TrayTrend', msg);
end;

procedure fixAlertPos;
var
  f: pointer;
  t, h: integer;
begin
  t := 25;
  try
  if (activewins <> nil) and (activewins.Count = 1) then
     tfalert(activewins[0]).top := 25
  else if (activewins <> nil) and (activewins.Count > 1) then begin
    for f in activewins do begin
      if (f <> nil) and (tfalert(f).visible) then begin
        tfalert(f).Top := t;
        t := 5 + t + tfalert(f).Height;
      end;
    end;
  end;
  finally
  end;
end;

function getAlertPos: integer;
var
  f: pointer;
  t: integer;
begin
  t := 20;
  try
  if (activewins <> nil) and (activewins.Count > 1) then begin
    for f in activewins do begin
      if f <> nil then
        if tfalert(f).Top+tfalert(f).Height > t then
          t := tfalert(f).Top+tfalert(f).Height+5;
    end;
  end;

  finally
  end;
  result := t+5;
end;

procedure ttAlert(msg: string; pic: tbitmap; modal: boolean = false);
var
  al: TfAlert;
begin
  // Make object oriented?
  if activewins = nil then
     activewins := TList.Create;

  al := tfalert.Create(nil);
  with al do begin
   activewins.add(al);
   Caption := 'TrayTrend Alert';
   lblmsg.caption := msg;
   pnTrend.color := clWhite;
   imTrend.Transparent:= false;
   imTrend.Color := clwhite;
   imTrend.Picture.Bitmap := pic;
   al.Top := getAlertPos;
   if modal then ShowModal else Show;
end;

end;

function ttMsg(title, msg: string; btntxt: string = 'OK'; modal: boolean = true): boolean;
begin
  with TfInfo.Create(nil) do begin
    Caption := Title;
    mTxt.Caption := msg;
    btn.Caption := btntxt;
    if modal then ShowModal else Show;
  end;
end;

function ttMsg(title, msg: string; heightadd: integer; btntxt: string = 'OK'; modal: boolean = true): boolean;
begin
  with TfInfo.Create(nil) do begin
    if height > 0 then begin
      mtxt.height := mtxt.height+heightadd;
      btn.Top := btn.top+heightadd;
      height := height+heightadd;
      mTxt.Caption := msg;
    end else begin
      mTxt.Caption := msg;
      mTxt.AutoSize := true;
      btn.Top := height+mtxt.height+5;
      height := btn.top+btn.height+5;
    end;
    logo.Top := round((mtxt.top-mtxt.height)/2);
    Caption := Title;

    btn.Caption := btntxt;
    if modal then ShowModal else Show;
  end;
end;


end.

