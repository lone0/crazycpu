unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  TAGraph, TASeries, TATransformations, ucpuinfo, Math;  // Add Math unit here

type
  { TMainWindow }

  TMainWindow = class(TForm)
    Chart: TChart;
    RightAxisTransformations: TChartAxisTransformations;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    RightAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;

    MaxFreqSeries: TLineSeries;
    MinFreqSeries: TLineSeries;
    UsageSeries: TBarSeries;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FCPUManager: TCPUInfoManager;
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
  FCPUManager := TCPUInfoManager.Create;
  FTimePoint := 0;
  
  // Configure series
  MaxFreqSeries.AxisIndexY := 0;  // Left axis
  MinFreqSeries.AxisIndexY := 0;  // Left axis
  UsageSeries.AxisIndexY := 2; // Right axis
  
  MaxFreqSeries.SeriesColor := clRed;
  MinFreqSeries.SeriesColor := clBlue;
  UsageSeries.SeriesColor := clGreen;
  
  // Configure axis ranges
  Chart.AxisList[1].Range.Min := 0;    // Time axis
  Chart.AxisList[1].Range.Max := MinsToShow;

  UpdateTimer.Interval := 1000;
  UpdateTimer.Enabled := True;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FCPUManager.Free;
end;

procedure TMainWindow.UpdateTimerTimer(Sender: TObject);
var
  i: Integer;
  MaxFreq, MinFreq: Double;
  TotalUsage: Double;
begin
  Inc(FTimePoint);
  
  // Initialize min/max
  MaxFreq := 0;
  MinFreq := 999999;
  TotalUsage := 0;
  
  // Collect data from all cores
  for i := 0 to FCPUManager.CoreCount - 1 do
  begin
    with FCPUManager.Cores[i] do
    begin
      MaxFreq := Max(MaxFreq, GetCPUFrequency);
      MinFreq := Min(MinFreq, GetCPUFrequency);
      TotalUsage := TotalUsage + GetCPUUsage;
    end;
  end;
  
  // Plot the data
  MaxFreqSeries.AddXY(FTimePoint, MaxFreq);
  MinFreqSeries.AddXY(FTimePoint, MinFreq);
  UsageSeries.AddXY(FTimePoint, TotalUsage / FCPUManager.CoreCount);
  
  // Keep last MinsToShow seconds of data
  if FTimePoint > MinsToShow then
  begin
    MaxFreqSeries.Delete(0);
    MinFreqSeries.Delete(0);
    UsageSeries.Delete(0);
    Chart.AxisList[1].Range.Min := FTimePoint - MinsToShow;
    Chart.AxisList[1].Range.Max := FTimePoint;

  end;
end;

end.

