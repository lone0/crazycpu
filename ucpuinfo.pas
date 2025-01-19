unit ucpuinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, DateUtils;

type
  TCPUInfo = class
  private
    FCoreID: Integer;
    FPrevIdleTime: Int64;
    FPrevTotalTime: Int64;
    FCPUFrequency: Double;
    FCPUUsage: Double;
  public
    constructor Create(ACoreID: Integer);
    function GetCPUFrequency: Double;
    function GetCPUUsage: Double;
    property CoreID: Integer read FCoreID;
    property CPUFrequency: Double read FCPUFrequency;
    property CPUUsage: Double read FCPUUsage;
  end;

  TCPUInfoManager = class
  private
    FCores: TList;
    FCoreCount: Integer;
    function GetCore(Index: Integer): TCPUInfo;
    function DetectCoreCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Cores[Index: Integer]: TCPUInfo read GetCore;
    property CoreCount: Integer read FCoreCount;
  end;

implementation

{ TCPUInfo }

constructor TCPUInfo.Create(ACoreID: Integer);
begin
  inherited Create;
  FCoreID := ACoreID;
  FPrevIdleTime := 0;
  FPrevTotalTime := 0;
  FCPUFrequency := 0;
  FCPUUsage := 0;
end;

function TCPUInfo.GetCPUFrequency: Double;
var
  ProcessOutput: TStringList;
  FreqFile: String;
begin
  ProcessOutput := TStringList.Create;
  try
    FreqFile := Format('/sys/devices/system/cpu/cpu%d/cpufreq/scaling_cur_freq', [FCoreID]);
    ProcessOutput.LoadFromFile(FreqFile);
    if ProcessOutput.Count > 0 then
      FCPUFrequency := StrToFloat(ProcessOutput[0]) / 1000; // KHz to MHz
  except
    FCPUFrequency := 0;
  end;
  ProcessOutput.Free;
  WriteLn(Format('[CPU%d] %s - Freq: %.2f MHz',
      [FCoreID, FormatDateTime('hh:nn:ss.zzz', Now), FCPUFrequency]));
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
    if ProcStat.Count > FCoreID + 1 then  // +1 because cpu0 is on line 1
    begin
      Values.Delimiter := ' ';
      Values.DelimitedText := ProcStat[FCoreID + 1];  // Get specific core stats
      
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
        WriteLn(Format('[CPU%d] %s - Usage: %.2f%%',
               [FCoreID, FormatDateTime('hh:nn:ss.zzz', Now), FCPUUsage]));
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

{ TCPUInfoManager }

constructor TCPUInfoManager.Create;
var
  i: Integer;
begin
  inherited Create;
  FCores := TList.Create;
  FCoreCount := DetectCoreCount;
  
  // Create CPU info objects for each core
  for i := 0 to FCoreCount - 1 do
    FCores.Add(TCPUInfo.Create(i));
end;

destructor TCPUInfoManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCores.Count - 1 do
    TCPUInfo(FCores[i]).Free;
  FCores.Free;
  inherited;
end;

function TCPUInfoManager.GetCore(Index: Integer): TCPUInfo;
begin
  if (Index >= 0) and (Index < FCores.Count) then
    Result := TCPUInfo(FCores[Index])
  else
    Result := nil;
end;

function TCPUInfoManager.DetectCoreCount: Integer;
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := 1; // Default to 1 core
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'nproc';
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;
    Output.LoadFromStream(Process.Output);
    if Output.Count > 0 then
      Result := StrToIntDef(Output[0], 1);
  finally
    Process.Free;
    Output.Free;
  end;
end;

end.
