unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  TAGraph, TASeries, TATransformations, ucpuinfo;

type
  { TMainWindow }

  TMainWindow = class(TForm)
    Chart: TChart;
    RightAxisTransformations: TChartAxisTransformations;
    LeftAxisTransformations: TChartAxisTransformations;

    FreqSeries: TLineSeries;
    LeftAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    RightAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
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
  
  // Configure series
  FreqSeries.AxisIndexY := 0;  // Left axis
  UsageSeries.AxisIndexY := 2; // Right axis
  
  FreqSeries.SeriesColor := clBlue;
  UsageSeries.SeriesColor := clRed;
  
  // Configure axis ranges
  Chart.AxisList[1].Range.Min := 0;    // Time axis
  Chart.AxisList[1].Range.Max := MinsToShow;

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

  end;
end;

end.

