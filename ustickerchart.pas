unit ustickerchart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, TAGraph;

type
  TStickerChart = class(TChart)
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  end;

implementation

function TStickerChart.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  ScrollBox: TScrollBox;
begin
  Result := False;
  if Parent is TScrollBox then
  begin
    ScrollBox := TScrollBox(Parent);
    with ScrollBox.VertScrollBar do
      Position := Position - WheelDelta div abs(WheelDelta) * Increment;
    Result := True;
  end;
end;

end.
