unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, TAGraph, TASeries, ucpuinfo;

type
  { TMainWindow }

  TMainWindow = class(TForm)
    SampleBtn: TButton;
    Chart: TChart;
    FreqSeries: TLineSeries;
    UsageSeries: TBarSeries;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SampleBtnClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FCPUInfo: TCPUInfo;
    FTimePoint: Integer;
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
  
  Chart.LeftAxis.Title.Caption := 'CPU Frequency (MHz)';
  Chart.LeftAxis2.Title.Caption := 'CPU Usage (%)';
  Chart.BottomAxis.Title.Caption := 'Time';
  
  UpdateTimer.Interval := 1000;
  UpdateTimer.Enabled := True;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FCPUInfo.Free;
end;

procedure TMainWindow.SampleBtnClick(Sender: TObject);
begin

end;

procedure TMainWindow.UpdateTimerTimer(Sender: TObject);
begin
  Inc(FTimePoint);
  
  FreqSeries.AddXY(FTimePoint, FCPUInfo.GetCPUFrequency);
  UsageSeries.AddXY(FTimePoint, FCPUInfo.GetCPUUsage);
  
  if FTimePoint > 60 then
  begin
    FreqSeries.Delete(0);
    UsageSeries.Delete(0);
    Chart.BottomAxis.Minimum := FTimePoint - 60;
    Chart.BottomAxis.Maximum := FTimePoint;
  end;
end;

end.

