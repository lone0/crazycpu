unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, TAGraph, TASeries, TATransformations, ucpuinfo;  // Add Math unit here

type
  { TMainWindow }

  TMainWindow = class(TForm)
    OverallChart: TChart;
    GroupBox1: TGroupBox;
    RightAxisTransformations: TChartAxisTransformations;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    RightAxisTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;

    MaxFreqSeries: TLineSeries;
    MinFreqSeries: TLineSeries;
    MainStatusBar: TStatusBar;
    UsageSeries: TBarSeries;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
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

  // Configure axis ranges
  OverallChart.AxisList[1].Range.Min := 0;    // Time axis
  OverallChart.AxisList[1].Range.Max := MinsToShow;

  // Configure axis ranges based on CPU capabilities
  OverallChart.AxisList[0].Range.Min := FCPUManager.MinMHz;
  OverallChart.AxisList[0].Range.Max := FCPUManager.MaxMHz;
  OverallChart.AxisList[0].Range.UseMin := True;
  OverallChart.AxisList[0].Range.UseMax := True;

  UpdateTimer.Interval := 1000;
  UpdateTimer.Enabled := True;

  MainStatusBar.SimpleText := Format('Total Core: %d', [FCPUManager.CoreCount]);
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FCPUManager.Free;
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  OverallChart.Height := MainWindow.ClientHeight div 2;
end;

procedure TMainWindow.UpdateTimerTimer(Sender: TObject);
var
  i: Integer;
  MaxFreq, MinFreq: Double;
  TotalUsage: Double;
  MaxFreqCore, MinFreqCore: Integer;
  CurrentFreq: Double;
begin
  Inc(FTimePoint);
  
  // Initialize min/max
  MaxFreq := 0;
  MinFreq := 999999;
  TotalUsage := 0;
  MaxFreqCore := 0;
  MinFreqCore := 0;
  
  // Collect data from all cores
  for i := 0 to FCPUManager.CoreCount - 1 do
  begin
    with FCPUManager.Cores[i] do
    begin
      CurrentFreq := GetCPUFrequency;
      if CurrentFreq > MaxFreq then
      begin
        MaxFreq := CurrentFreq;
        MaxFreqCore := i;
      end;
      if CurrentFreq < MinFreq then
      begin
        MinFreq := CurrentFreq;
        MinFreqCore := i;
      end;
      TotalUsage := TotalUsage + GetCPUUsage;
    end;
  end;
  
  // Update legends with core numbers
  MaxFreqSeries.Legend.Format := Format('Max Freq (CPU%d)', [MaxFreqCore]);
  MinFreqSeries.Legend.Format := Format('Min Freq (CPU%d)', [MinFreqCore]);
  
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
    OverallChart.AxisList[1].Range.Min := FTimePoint - MinsToShow;
    OverallChart.AxisList[1].Range.Max := FTimePoint;
  end;
end;

end.

