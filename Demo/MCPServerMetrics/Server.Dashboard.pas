unit Server.Dashboard;

{
  ==============================================================================
   MCPConnect - MCPServerMetrics showcase: the metrics dashboard (frame)
  ==============================================================================

  This is the screen half of the showcase, and it is a plain VCL frame: every
  control lives in Server.Dashboard.dfm, so the layout can be seen and changed
  in the IDE designer (open this unit and press F12, or right-click the unit in
  the Project Manager and pick "View as Form"). Host forms only drop the frame
  and call RefreshAll once the metrics hub has started.

  Layout:

    About        what the demo is and where the code is
    Metrics      the live grid - one row per series - plus the action buttons
    Use cases    the narrative the demo is built around
    Slowest      the slowest histograms, refreshed on every harvest
    Text         the latest harvest rendered by MetricsToText
    JSON         the latest harvest rendered by MetricsToJson
    Exporters    the exporter list and the series-per-harvest history
    Log          the application log (this frame owns the memo the buffer
                 adapter writes into)

  Behaviour: the list view reads TMetrics.Collect on a one second timer while
  its tab is visible and "Live" is checked, so it shows the accumulation as it
  happens without disturbing it. Host gauges are sampled on a slower timer; the
  delta harvest that feeds the files stays on the host form's 30 s timer.

  The frame talks to the process-wide TServerMetrics hub directly: a frame is
  instantiated by the IDE at design time as well as by the host at run time, so
  there is no constructor argument to pass one in. At design time the hub is left
  alone (nothing is created in the IDE process) and the timers never fire.
}

interface

uses
  System.Classes, System.SysUtils,

  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  MCPConnect.Metrics,
  Server.Metrics;

type
  /// <summary>
  ///   The tabbed metrics dashboard. Drop it on a form, align it, and call
  ///   RefreshAll after TServerMetrics.Instance.Start has attached the
  ///   exporters, so the memos the exporters write into are the ones on screen.
  /// </summary>
  TFrameMetricsDashboard = class(TFrame)
    Pages: TPageControl;
    TabAbout: TTabSheet;
    MemoAbout: TMemo;
    TabMetrics: TTabSheet;
    Grid: TListView;
    PanelActions: TPanel;
    ChkLive: TCheckBox;
    BtnHarvestDelta: TButton;
    BtnHarvestCumulative: TButton;
    BtnSample: TButton;
    BtnClear: TButton;
    BtnWorkload: TButton;
    BtnErrors: TButton;
    BtnCardinality: TButton;
    BtnProvider: TButton;
    LabelHint: TLabel;
    TabCases: TTabSheet;
    MemoCases: TMemo;
    TabSlowest: TTabSheet;
    MemoSlowest: TMemo;
    TabText: TTabSheet;
    MemoText: TMemo;
    TabJson: TTabSheet;
    MemoJson: TMemo;
    TabExporters: TTabSheet;
    MemoExporters: TMemo;
    TabLog: TTabSheet;
    MemoLog: TMemo;
    UiTimer: TTimer;
    SampleTimer: TTimer;
    procedure UiTimerTimer(Sender: TObject);
    procedure SampleTimerTimer(Sender: TObject);
    procedure BtnHarvestDeltaClick(Sender: TObject);
    procedure BtnHarvestCumulativeClick(Sender: TObject);
    procedure BtnSampleClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnWorkloadClick(Sender: TObject);
    procedure BtnErrorsClick(Sender: TObject);
    procedure BtnCardinalityClick(Sender: TObject);
    procedure BtnProviderClick(Sender: TObject);
  private
    FHub: TServerMetrics;
    FTicks: Integer;
    procedure RefreshGrid;
    procedure RefreshExporters;
    procedure ShowNarrative(const AText: string);
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Fills the static tabs and starts the timers. Call it after the host
    ///   has started the hub, so the exporter views write into live memos.
    /// </summary>
    procedure RefreshAll;

    /// <summary>Stops the timers (called while the host is shutting down).</summary>
    procedure Deactivate;

    /// <summary>The memo the application log goes to, for the buffer adapter.</summary>
    function LogLines: TStrings;

    /// <summary>Where the latest-harvest exporters write.</summary>
    function TextTarget: TStrings;
    function JsonTarget: TStrings;
    function SlowestTarget: TStrings;
  end;

implementation

{$R *.dfm}

const
  GRID_REFRESH_MS = 1000;
  SAMPLE_REFRESH_MS = 3000;
  EXPORTERS_REFRESH_TICKS = 5;
  MAX_GRID_ROWS = 600;

{ TFrameMetricsDashboard }

constructor TFrameMetricsDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // At design time the IDE instantiates the frame too; leave the process-wide
  // hub alone there. Every handler below tolerates a nil hub.
  if not (csDesigning in ComponentState) then
    FHub := TServerMetrics.Instance;
end;

procedure TFrameMetricsDashboard.RefreshAll;
begin
  if FHub = nil then
    Exit;

  MemoAbout.Text := FHub.AboutText;
  MemoCases.Text := FHub.UseCasesReport;
  RefreshExporters;
  RefreshGrid;

  UiTimer.Interval := GRID_REFRESH_MS;
  UiTimer.Enabled := True;
  SampleTimer.Interval := SAMPLE_REFRESH_MS;
  SampleTimer.Enabled := True;
end;

procedure TFrameMetricsDashboard.Deactivate;
begin
  UiTimer.Enabled := False;
  SampleTimer.Enabled := False;
end;

function TFrameMetricsDashboard.LogLines: TStrings;
begin
  Result := MemoLog.Lines;
end;

function TFrameMetricsDashboard.TextTarget: TStrings;
begin
  Result := MemoText.Lines;
end;

function TFrameMetricsDashboard.JsonTarget: TStrings;
begin
  Result := MemoJson.Lines;
end;

function TFrameMetricsDashboard.SlowestTarget: TStrings;
begin
  Result := MemoSlowest.Lines;
end;

{ Timers ------------------------------------------------------------------- }

procedure TFrameMetricsDashboard.UiTimerTimer(Sender: TObject);
begin
  if FHub = nil then
    Exit;

  Inc(FTicks);
  if not FHub.Active then
    Exit;

  if (Pages.ActivePage = TabMetrics) and ChkLive.Checked then
    RefreshGrid;

  if (Pages.ActivePage = TabExporters) and ((FTicks mod EXPORTERS_REFRESH_TICKS) = 0) then
    RefreshExporters;
end;

procedure TFrameMetricsDashboard.SampleTimerTimer(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  FHub.SampleSystem;
end;

{ Grid --------------------------------------------------------------------- }

procedure TFrameMetricsDashboard.RefreshGrid;
var
  LPoints: TArray<TMetricPoint>;
  LPoint: TMetricPoint;
  LItem: TListItem;
  LAvg: Double;
begin
  if FHub = nil then
    Exit;

  LPoints := TMetrics.Collect;
  Grid.Items.BeginUpdate;
  try
    Grid.Items.Clear;
    for LPoint in LPoints do
    begin
      if Grid.Items.Count >= MAX_GRID_ROWS then
        Break;

      LItem := Grid.Items.Add;
      LItem.Caption := LPoint.Meter;
      LItem.SubItems.Add(LPoint.Name);
      LItem.SubItems.Add(MetricKindToStr(LPoint.Kind));
      LItem.SubItems.Add(LabelsToString(LPoint.Labels));
      LItem.SubItems.Add(IntToStr(LPoint.Count));

      case LPoint.Kind of
        TMetricKind.Counter:
          begin
            LItem.SubItems.Add(FormatFloat('0.###', LPoint.Sum, TFormatSettings.Invariant));
            LItem.SubItems.Add('');
          end;
        TMetricKind.Gauge:
          begin
            LItem.SubItems.Add(FormatFloat('0.###', LPoint.Last, TFormatSettings.Invariant));
            LItem.SubItems.Add('');
          end;
      else
        if LPoint.Count > 0 then
          LAvg := LPoint.Sum / LPoint.Count
        else
          LAvg := 0;
        LItem.SubItems.Add(FormatFloat('0.###', LAvg, TFormatSettings.Invariant));
        LItem.SubItems.Add(Format('min %s / max %s', [
          FormatFloat('0.###', LPoint.Min, TFormatSettings.Invariant),
          FormatFloat('0.###', LPoint.Max, TFormatSettings.Invariant)]));
      end;

      LItem.SubItems.Add(LPoint.UnitName);
    end;
  finally
    Grid.Items.EndUpdate;
  end;
end;

procedure TFrameMetricsDashboard.RefreshExporters;
begin
  if FHub = nil then
    Exit;
  MemoExporters.Text := FHub.ExportersReport;
end;

procedure TFrameMetricsDashboard.ShowNarrative(const AText: string);
begin
  // The two API demonstrations are narratives, not points: show them in the
  // Text tab, and switch there so the press has a visible effect.
  MemoText.Text := AText;
  Pages.ActivePage := TabText;
end;

{ Buttons ------------------------------------------------------------------ }

procedure TFrameMetricsDashboard.BtnHarvestDeltaClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  FHub.HarvestDelta;
  RefreshExporters;
  RefreshGrid;
end;

procedure TFrameMetricsDashboard.BtnHarvestCumulativeClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  FHub.HarvestCumulative;
  RefreshExporters;
  RefreshGrid;
end;

procedure TFrameMetricsDashboard.BtnSampleClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  FHub.SampleSystem;
  RefreshGrid;
end;

procedure TFrameMetricsDashboard.BtnClearClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  TMetrics.Clear;
  RefreshGrid;
end;

procedure TFrameMetricsDashboard.BtnWorkloadClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  FHub.SimulateWorkload(50, 'eu,us,apac');
  RefreshGrid;
end;

procedure TFrameMetricsDashboard.BtnErrorsClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  FHub.SimulateErrors(10);
  RefreshGrid;
end;

procedure TFrameMetricsDashboard.BtnCardinalityClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  ShowNarrative(FHub.CardinalityDemo);
end;

procedure TFrameMetricsDashboard.BtnProviderClick(Sender: TObject);
begin
  if FHub = nil then
    Exit;
  ShowNarrative(FHub.SeparateProviderDemo);
end;

end.
