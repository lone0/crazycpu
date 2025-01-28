unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, TAGraph, TASeries, TATransformations, TAChartAxisUtils, ucpuinfo;  // Add Math unit here

type
  { TMainWindow }

  TCoreChart = record
    Chart: TChart;
    FreqSeries: TLineSeries;
    UsageSeries: TBarSeries;
  end;

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
    FCoreCharts: array of TCoreChart;
    const
      MinsToShow = 60;
      CHARTS_PER_ROW = 2;
      CHART_WIDTH = 400;
      CHART_HEIGHT = 200;
      CHART_MARGIN = 10;
    procedure CreateCoreCharts;
    procedure ConfigureChart(AChart: TChart; ATitle: string);
    procedure UpdateCoreChart(ACoreChart: TCoreChart; ACPUInfo: TCPUInfo);
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
  OverallChart.AxisList[0].Range.Min := FCPUManager.MinMHz * 0.75;
  OverallChart.AxisList[0].Range.Max := FCPUManager.MaxMHz + FCPUManager.MinMHz / 4;
  OverallChart.AxisList[0].Range.UseMin := True;
  OverallChart.AxisList[0].Range.UseMax := True;

  UpdateTimer.Interval := 1000;
  UpdateTimer.Enabled := True;

  MainStatusBar.SimpleText := Format('Total Core: %d', [FCPUManager.CoreCount]);

  CreateCoreCharts;
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

  // Update individual core charts
  for i := 0 to High(FCoreCharts) do
    UpdateCoreChart(FCoreCharts[i], FCPUManager.Cores[i]);
end;

procedure TMainWindow.CreateCoreCharts;
var
  i, row, col: Integer;
  NewChart: TChart;
  ScrollBox: TScrollBox;
  TotalHeight: Integer;
begin
  SetLength(FCoreCharts, FCPUManager.CoreCount);
  
  // Create scrollbox in groupbox
  ScrollBox := TScrollBox.Create(GroupBox1);
  ScrollBox.Parent := GroupBox1;
  ScrollBox.Align := alClient;
  ScrollBox.HorzScrollBar.Visible := False;
  
  // Calculate total height needed
  TotalHeight := ((FCPUManager.CoreCount + CHARTS_PER_ROW - 1) div CHARTS_PER_ROW) 
                 * (CHART_HEIGHT + CHART_MARGIN);
  
  for i := 0 to FCPUManager.CoreCount - 1 do
  begin
    row := i div CHARTS_PER_ROW;
    col := i mod CHARTS_PER_ROW;
    
    // Create chart
    NewChart := TChart.Create(ScrollBox);
    with NewChart do
    begin
      Parent := ScrollBox;
      Left := col * (CHART_WIDTH + CHART_MARGIN);
      Top := row * (CHART_HEIGHT + CHART_MARGIN);
      Width := CHART_WIDTH;
      Height := CHART_HEIGHT;
    end;
    
    // Configure chart
    ConfigureChart(NewChart, Format('Core %d', [FCPUManager.Cores[i].CPUNumber]));
    
    // Create series
    with FCoreCharts[i] do
    begin
      Chart := NewChart;
      
      FreqSeries := TLineSeries.Create(Chart);
      with FreqSeries do
      begin
        Chart.AddSeries(FreqSeries);
        AxisIndexY := 0;
        SeriesColor := clBlue;
        Title := 'Frequency';
      end;
      
      UsageSeries := TBarSeries.Create(Chart);
      with UsageSeries do
      begin
        Chart.AddSeries(UsageSeries);
        AxisIndexY := 2;
        SeriesColor := clSilver;
        Title := 'Usage';
      end;
    end;
  end;
end;

procedure TMainWindow.ConfigureChart(AChart: TChart; ATitle: string);
var
  LeftTrans, RightTrans: TChartAxisTransformations;
begin
  with AChart do
  begin
    Title.Text.Text := ATitle;
    Hint := ATitle;
    ShowHint := true;
    
    // Create transformations
    LeftTrans := TChartAxisTransformations.Create(self);
    RightTrans := TChartAxisTransformations.Create(self);
    
    // Add auto-scale transforms
    TAutoScaleAxisTransform.Create(LeftTrans).Transformations := LeftTrans;
    TAutoScaleAxisTransform.Create(RightTrans).Transformations := RightTrans;
    
    // Left axis (Frequency)
    //AxisList.Add.Alignment := calLeft;
    with AxisList[0] do
    begin
      Alignment := calLeft;
      Title.Caption := 'MHz';
      Range.Min := FCPUManager.MinMHz;
      Range.Max := FCPUManager.MaxMHz;
      Range.UseMin := True;
      Range.UseMax := True;
      Transformations := LeftTrans;
    end;
    
    // Bottom axis (Time)
    //AxisList.Add.Alignment := calBottom;
    AxisList[1].Title.Caption := 'Time';
    
    // Right axis (Usage)
    AxisList.Add.Alignment := calRight;
    with AxisList[2] do
    begin
      Alignment := calRight;
      Title.Caption := '%';
      Range.Min := 0;
      Range.Max := 100;
      Range.UseMin := True;
      Range.UseMax := True;
      Transformations := RightTrans;
    end;
  end;
end;

procedure TMainWindow.UpdateCoreChart(ACoreChart: TCoreChart; ACPUInfo: TCPUInfo);
begin
  with ACoreChart do
  begin
    FreqSeries.AddXY(FTimePoint, ACPUInfo.GetCPUFrequency);
    UsageSeries.AddXY(FTimePoint, ACPUInfo.GetCPUUsage);
    
    if FTimePoint > MinsToShow then
    begin
      FreqSeries.Delete(0);
      UsageSeries.Delete(0);
      Chart.AxisList[1].Range.Min := FTimePoint - MinsToShow;
      Chart.AxisList[1].Range.Max := FTimePoint;
    end;
  end;
end;

end.

