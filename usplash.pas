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
unit usplash;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls,
  ExtCtrls;

type

  { TSplash }

  TSplash = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
  private

  public

  end;

var
  Splash: TSplash;

implementation

{$R *.lfm}

{ TSplash }

end.

