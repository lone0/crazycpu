unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  TAGraph, TASeries, ucpuinfo;

type
  { TMainWindow }

  TMainWindow = class(TForm)
    Chart: TChart;
    FreqSeries: TLineSeries;
    UsageSeries: TBarSeries;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FCPUInfo: TCPUInfo;
    FTimePoint: Integer;
    const
      MinsToShow = 60;  // Show 60 seconds of data
  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FCPUInfo := TCPUInfo.Create;
  FTimePoint := 0;
  
  FreqSeries.SeriesColor := clBlue;
  UsageSeries.SeriesColor := clRed;
  
  // Configure initial axis ranges
  Chart.AxisList[0].Range.Min := 1000;    // Frequency axis
  Chart.AxisList[0].Range.Max := 5000; // Assume max 5GHz
  Chart.AxisList[1].Range.Min := 0;    // Time axis
  Chart.AxisList[1].Range.Max := MinsToShow;
  Chart.AxisList[2].Range.Min := 0;    // Usage axis
  Chart.AxisList[2].Range.Max := 100;  // Percentage
  
  UpdateTimer.Interval := 1000;
  UpdateTimer.Enabled := True;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FCPUInfo.Free;
end;

procedure TMainWindow.UpdateTimerTimer(Sender: TObject);
begin
  Inc(FTimePoint);
  
  FreqSeries.AddXY(FTimePoint, FCPUInfo.GetCPUFrequency);
  UsageSeries.AddXY(FTimePoint, FCPUInfo.GetCPUUsage);
  
  // Keep last MinsToShow seconds of data
  if FTimePoint > MinsToShow then
  begin
    FreqSeries.Delete(0);
    UsageSeries.Delete(0);
    Chart.AxisList[1].Range.Min := FTimePoint - MinsToShow;
    Chart.AxisList[1].Range.Max := FTimePoint;
    Chart.AxisList[2].Range.Min := 0;    // Usage axis
    Chart.AxisList[2].Range.Max := 100;  // Percentage
  end;
end;

end.

