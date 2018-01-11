unit uhover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfHover }

  TfHover = class(TForm)
    lblVal: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public
    trans: integer;

  end;

var
  fHover: TfHover;

implementation

{$R *.lfm}

{ TfHover }

procedure TfHover.FormShow(Sender: TObject);
begin
  self.AlphaBlendValue := trans; // 0..255
  self.AlphaBlend := True;

end;

end.

