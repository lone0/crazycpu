unit ucpuinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, DateUtils;

type
  TCoreType = (ctPhysical, ctLogical);

  TCPUInfo = class
  private
    FCPUNumber: Integer;    // CPU logical number (0-N)
    FCoreNumber: Integer;   // Core number within socket
    FSocketNumber: Integer; // Physical socket number
    FPrevIdleTime: Int64;
    FPrevTotalTime: Int64;
    FCPUFrequency: Double;
    FCPUUsage: Double;
  public
    constructor Create(ACPUNumber, ACoreNumber, ASocketNumber: Integer);
    function GetCPUFrequency: Double;
    function GetCPUUsage: Double;
    property CPUNumber: Integer read FCPUNumber;
    property CoreNumber: Integer read FCoreNumber;
    property SocketNumber: Integer read FSocketNumber;
    property CPUFrequency: Double read FCPUFrequency;
    property CPUUsage: Double read FCPUUsage;
  end;

  TCPUInfoManager = class
  private
    FCores: TList;
    FCoreCount: Integer;
    FMaxMHz: Double;
    FMinMHz: Double;
    function GetCore(Index: Integer): TCPUInfo;
    function ParseCPUInfo: Integer;
    procedure ParseCPUFreqRange;
  public
    constructor Create;
    destructor Destroy; override;
    property Cores[Index: Integer]: TCPUInfo read GetCore;
    property CoreCount: Integer read FCoreCount;
    property MaxMHz: Double read FMaxMHz;
    property MinMHz: Double read FMinMHz;
  end;

implementation

{ TCPUInfo }

constructor TCPUInfo.Create(ACPUNumber, ACoreNumber, ASocketNumber: Integer);
begin
  inherited Create;
  FCPUNumber := ACPUNumber;     // CPU number from lscpu
  FCoreNumber := ACoreNumber;   // Core number within socket
  FSocketNumber := ASocketNumber; // Physical socket number
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
    FreqFile := Format('/sys/devices/system/cpu/cpu%d/cpufreq/scaling_cur_freq', [FCPUNumber]);
    ProcessOutput.LoadFromFile(FreqFile);
    if ProcessOutput.Count > 0 then
      FCPUFrequency := StrToFloat(ProcessOutput[0]) / 1000; // KHz to MHz
  except
    FCPUFrequency := 0;
  end;
  ProcessOutput.Free;
  WriteLn(Format('[CPU%d] %s - Freq: %.2f MHz',
      [FCPUNumber, FormatDateTime('hh:nn:ss.zzz', Now), FCPUFrequency]));
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
    if ProcStat.Count > FCPUNumber + 1 then  // +1 because cpu0 is on line 1
    begin
      Values.Delimiter := ' ';
      Values.DelimitedText := ProcStat[FCPUNumber + 1];  // Get specific core stats
      
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
               [FCPUNumber, FormatDateTime('hh:nn:ss.zzz', Now), FCPUUsage]));
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
begin
  inherited Create;
  FCores := TList.Create;
  ParseCPUFreqRange;
  FCoreCount := ParseCPUInfo;
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

function TCPUInfoManager.ParseCPUInfo: Integer;
var
  Process: TProcess;
  Output: TStringList;
  UniqueKeys: TStringList;
  Line: String;
  Fields: TStringArray;
  Key: String;
  CPU, Core, Socket: Integer;
  NewCPUInfo: TCPUInfo;
begin
  Result := 0;
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  UniqueKeys := TStringList.Create;
  
  try
    Process.Executable := 'lscpu';
    Process.Parameters.Add('--parse=CPU,Core,Socket,Node');
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;
    
    Output.LoadFromStream(Process.Output);
    WriteLn(Format('Found %d lines in lscpu output', [Output.Count]));
    
    for Line in Output do
    begin
      // Skip comments
      if (Length(Line) = 0) or (Line[1] = '#') then
        Continue;
        
      Fields := Line.Split(',');
      if Length(Fields) >= 3 then
      begin
        CPU := StrToIntDef(Fields[0], -1);
        Core := StrToIntDef(Fields[1], -1);
        Socket := StrToIntDef(Fields[2], -1);
        
        // Create unique key from Core+Socket
        Key := Format('%d:%d', [Socket, Core]);
        
        // Only process unique combinations
        if (UniqueKeys.IndexOf(Key) = -1) and (CPU >= 0) and (Core >= 0) and (Socket >= 0) then
        begin
          UniqueKeys.Add(Key);
          WriteLn(Format('Found unique core: CPU=%d, Core=%d, Socket=%d', [CPU, Core, Socket]));
          
          NewCPUInfo := TCPUInfo.Create(CPU, Core, Socket);
          FCores.Add(NewCPUInfo);
          Inc(Result);
        end;
      end;
    end;
    
    WriteLn(Format('Total unique cores found: %d', [Result]));
    
  finally
    Process.Free;
    Output.Free;
    UniqueKeys.Free;
  end;
end;

procedure TCPUInfoManager.ParseCPUFreqRange;
var
  Process: TProcess;
  Output: TStringList;
  Line: String;
begin
  FMaxMHz := 5000;  // Default fallback
  FMinMHz := 0;
  
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'lscpu';
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;
    
    Output.LoadFromStream(Process.Output);
    for Line in Output do
    begin
      if Pos('CPU max MHz:', Line) > 0 then
        FMaxMHz := StrToFloatDef(Copy(Line, Pos(':', Line) + 1, Length(Line)), 5000)
      else if Pos('CPU min MHz:', Line) > 0 then
        FMinMHz := StrToFloatDef(Copy(Line, Pos(':', Line) + 1, Length(Line)), 0);
    end;
    WriteLn(Format('CPU Frequency Range: %.1f - %.1f MHz', [FMinMHz, FMaxMHz]));
  finally
    Process.Free;
    Output.Free;
  end;
end;

end.
