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
program TrayTrend;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, umain, uconfig, usplash, usys, uhover, uInfo, stuff,
  ualert
  { you can add units after this };

{$R *.res}

begin
  Splash := TSplash.Create(nil);
  RequireDerivedFormResource:=True;
  Application.Initialize;
  splash.show;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfSettings, fSettings);
  if assigned(Splash) then
    Splash.Close;
  splash.free;
  Application.CreateForm(TfSysSettings, fSysSettings);
  Application.CreateForm(TfHover, fHover);
  Application.Run;
end.

