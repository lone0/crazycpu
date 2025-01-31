unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, TAGraph, TASeries, TATransformations, TAChartAxisUtils, TADrawUtils,
  ucpuinfo, Math;  // Add Math unit here

type
  { TMainWindow }

  TCoreChart = record
    Chart: TChart;
    FreqSeries: TAreaSeries;
    UsageSeries: TBarSeries;
  end;

  TMainWindow = class(TForm)
    OverallChart: TChart;
    GroupBox1: TGroupBox;
    CoreScroll: TScrollBox;
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
      CHART_WIDTH = 160;
      CHART_HEIGHT = 120;
      CHART_MARGIN = 2;
    procedure CreateCoreCharts;
    procedure ConfigureChart(AChart: TChart; ATitle: string);
    procedure UpdateCoreChart(ACoreChart: TCoreChart; ACPUInfo: TCPUInfo);
    procedure ArrangeCoreCharts;
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
  // ScrollBox will be freed automatically as it's owned by GroupBox1
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  // Adjust overall chart height
  OverallChart.Height := MainWindow.ClientHeight div 2;
  
  // Rearrange core charts
  ArrangeCoreCharts;
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
  i: Integer;
  NewChart: TChart;
begin
  SetLength(FCoreCharts, FCPUManager.CoreCount);
  
  for i := 0 to FCPUManager.CoreCount - 1 do
  begin
    NewChart := TChart.Create(CoreScroll);
    with NewChart do
    begin
      Parent := CoreScroll;
      Width := CHART_WIDTH;
      Height := CHART_HEIGHT;
      AntialiasingMode := amOn;
    end;
    
    // Configure chart
    ConfigureChart(NewChart, Format('Core %d', [FCPUManager.Cores[i].CPUNumber]));
    
    // Create series
    with FCoreCharts[i] do
    begin
      Chart := NewChart;
      
      FreqSeries := TAreaSeries.Create(Chart);
      with FreqSeries do
      begin
        Chart.AddSeries(FreqSeries);
        AxisIndexY := 0;
        AreaContourPen.Color := $e19455;
        Title := 'Frequency';
        AreaBrush.Color := $00EDBE96;
        AreaLinesPen.Style := psClear;
        Transparency := 200;
      end;
      
      UsageSeries := TBarSeries.Create(Chart);
      with UsageSeries do
      begin
        Chart.AddSeries(UsageSeries);
        AxisIndexY := 2;
        SeriesColor := clSilver;
        Title := 'Usage';
        ZPosition := 2;
      end;
    end;
  end;

  // Do initial arrangement
  ArrangeCoreCharts;
end;

procedure TMainWindow.ConfigureChart(AChart: TChart; ATitle: string);
var
  LeftTrans, RightTrans: TChartAxisTransformations;
begin
  with AChart do
  begin
    Title.Text.Text := ATitle;

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
      Marks.Visible := False;
    end;
    
    // Bottom axis (Time)
    with AxisList[1] do
    begin
      Title.Caption := 'Time';
      Marks.Visible := False;
    end;
    
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
      Marks.Visible := False;
    end;

    Foot.Text.Add(ATitle);
    Foot.Text.Add('0 %');
    Foot.Alignment := taLeftJustify;
    Foot.Visible := true; 
  end;
end;

procedure TMainWindow.UpdateCoreChart(ACoreChart: TCoreChart; ACPUInfo: TCPUInfo);
begin
  with ACoreChart do
  begin
    FreqSeries.AddXY(FTimePoint, ACPUInfo.GetCPUFrequency);
    UsageSeries.AddXY(FTimePoint, ACPUInfo.GetCPUUsage);
    Chart.Foot.Text[0] := Format('CPU %d: %.2f GHz', [ACPUInfo.CPUNumber, ACPUInfo.GetCPUFrequency/1000]);
    Chart.Foot.Text[1] := Format('%.1f %%', [ACPUInfo.CPUUsage]);
    
    if FTimePoint > MinsToShow then
    begin
      FreqSeries.Delete(0);
      UsageSeries.Delete(0);
      Chart.AxisList[1].Range.Min := FTimePoint - MinsToShow;
      Chart.AxisList[1].Range.Max := FTimePoint;
    end;
  end;
end;

procedure TMainWindow.ArrangeCoreCharts;
var
  i, row, col: Integer;
  AvailWidth: Integer;
  NewChartsPerRow: Integer;
  NewChartWidth: Integer;
  TotalRows: Integer;
begin
  if Length(FCoreCharts) = 0 then Exit;
  
  // Calculate available width
  AvailWidth := CoreScroll.ClientWidth;
  if CoreScroll.VertScrollBar.Visible then
    Dec(AvailWidth, CoreScroll.VertScrollBar.Size);
    
  // Calculate layout parameters
  NewChartsPerRow := Max(1, (AvailWidth - CHART_MARGIN) div (CHART_WIDTH + CHART_MARGIN));
  NewChartWidth := (AvailWidth - (NewChartsPerRow + 1) * CHART_MARGIN) div NewChartsPerRow;
  TotalRows := (Length(FCoreCharts) + NewChartsPerRow - 1) div NewChartsPerRow;
  
  // Arrange charts
  for i := 0 to High(FCoreCharts) do
  begin
    row := i div NewChartsPerRow;
    col := i mod NewChartsPerRow;
    
    with FCoreCharts[i].Chart do
    begin
      Left := CHART_MARGIN + col * (NewChartWidth + CHART_MARGIN);
      Top := CHART_MARGIN + row * (CHART_HEIGHT + CHART_MARGIN);
      Width := NewChartWidth;
      Height := CHART_HEIGHT;
    end;
  end;
end;

end.

