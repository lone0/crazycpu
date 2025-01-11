unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    SampleBtn: TButton;
    procedure SampleBtnClick(Sender: TObject);
  private

  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.SampleBtnClick(Sender: TObject);
begin

end;

end.

