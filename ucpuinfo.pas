unit ucpuinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, DateUtils;

type
  TCPUInfo = class
  private
    FPrevIdleTime: Int64;
    FPrevTotalTime: Int64;
    FCPUFrequency: Double;
    FCPUUsage: Double;
  public
    constructor Create;
    function GetCPUFrequency: Double;
    function GetCPUUsage: Double;
    property CPUFrequency: Double read FCPUFrequency;
    property CPUUsage: Double read FCPUUsage;
  end;

implementation

constructor TCPUInfo.Create;
begin
  inherited;
  FPrevIdleTime := 0;
  FPrevTotalTime := 0;
  FCPUFrequency := 0;
  FCPUUsage := 0;
end;

function TCPUInfo.GetCPUFrequency: Double;
var
  ProcessOutput: TStringList;
begin
  ProcessOutput := TStringList.Create;
  try
    ProcessOutput.LoadFromFile('/sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq');
    if ProcessOutput.Count > 0 then
      FCPUFrequency := StrToFloat(ProcessOutput[0]) / 1000; // KHz to MHz
  except
    FCPUFrequency := 0;
  end;
  ProcessOutput.Free;
  WriteLn(Format('[CPU] %s - Freq: %f',
      [FormatDateTime('hh:nn:ss.zzz', Now), FCPUFrequency]));
  Result := FCPUFrequency;
end;

function TCPUInfo.GetCPUUsage: Double;
var
  ProcStat: TStringList;
  Values: TStringList;
  User, Nice, System, Idle: Int64;
  Total, DiffIdle, DiffTotal: Int64;
begin
  Result := 0;
  ProcStat := TStringList.Create;
  Values := TStringList.Create;
  try
    ProcStat.LoadFromFile('/proc/stat');
    if ProcStat.Count > 0 then
    begin
      Values.Delimiter := ' ';
      Values.DelimitedText := ProcStat[0];
      
      User := StrToInt64(Values[1]);
      Nice := StrToInt64(Values[2]);
      System := StrToInt64(Values[3]);
      Idle := StrToInt64(Values[4]);
      
      Total := User + Nice + System + Idle;
      DiffIdle := Idle - FPrevIdleTime;
      DiffTotal := Total - FPrevTotalTime;
      
      if DiffTotal > 0 then
      begin
        FCPUUsage := 100.0 * (1.0 - DiffIdle / DiffTotal);
        WriteLn(Format('[CPU] %s - Usage: %.2f%%', 
               [FormatDateTime('hh:nn:ss.zzz', Now), FCPUUsage]));
      end;
      
      FPrevIdleTime := Idle;
      FPrevTotalTime := Total;
      Result := FCPUUsage;
    end;
  finally
    ProcStat.Free;
    Values.Free;
  end;
end;

end.
