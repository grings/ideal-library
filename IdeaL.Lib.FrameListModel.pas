unit IdeaL.Lib.FrameListModel;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,

  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,

  IdeaL.Lib.FrameItemListModel;

type
  TFrameListModel = class(TFrame)
    lytBackground: TLayout;
    vtsList: TScrollBox;
  private
    FObjList: TObjectList<TControl>;
    FClickItem: TNotifyEvent;
    FItemSelected: TControl;
    FFrameListOnDemand: TFrame;
    FOnDemandBoolean: TProc<Boolean>;
    FItemAlign: TAlignLayout;

    procedure ClickItem(Sender: TObject);
    procedure TapItem(Sender: TObject; const Point: TPointF);
    function GetChildrenCount: Integer;
    function GetContentHeight: Single;
    procedure SetItemSelected(const Value: TControl);
    procedure SetFrameListOnDemand(const Value: TFrame);
    procedure OnPaitingOnDemand(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SetOnDemandBoolean(const Value: TProc<Boolean>);

    procedure VtsListBeginUpdate;
    procedure VtsListEndUpdate;
    procedure SetItemAlign(const Value: TAlignLayout);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    property ChildrenCount: Integer read GetChildrenCount;
    property ContentHeight: Single read GetContentHeight;
    property OnClickItem: TNotifyEvent read FClickItem write FClickItem;
    property ObjList: TObjectList<TControl> read FObjList;

    procedure ClearList(ATarget: TFmxObject; AClickItem: TNotifyEvent); overload;
    procedure ClearList(); overload;
    function AddItem(AClass: TComponentClass): TFrameItemListModel;
    property ItemSelected: TControl read FItemSelected write SetItemSelected;
    property ItemAlign: TAlignLayout read FItemAlign write SetItemAlign;
    property FrameListOnDemand: TFrame read FFrameListOnDemand write SetFrameListOnDemand;

    property OnDemandBoolean: TProc<Boolean> read FOnDemandBoolean write SetOnDemandBoolean;
    { Public declarations }
  end;

implementation

{$R *.fmx}
{ TFrameListModel }

function TFrameListModel.AddItem(AClass: TComponentClass): TFrameItemListModel;
var
  LName: string;
  LCount: Integer;
begin
  LName := FormatDateTime('yyyymmddhhnnsszzz', Now);
  LCount := vtsList.Content.ChildrenCount;

  Result := (AClass.Create(vtsList) as TFrameItemListModel);
  Result.Name := Result.ClassName + LCount.ToString + '_' + LName;
  // Result.Parent := vtsList;
  Result.Align := ItemAlign;
  vtsList.AddObject(Result);
  Result.Position.Y := Result.Height * LCount;
  FObjList.Add(Result);
{$IFDEF MSWINDOWS}
  Result.SetDefaultClick(ClickItem);
{$ELSE}
  Result.SetDefaultTap(TapItem);
{$ENDIF}
end;

procedure TFrameListModel.ClickItem(Sender: TObject);
begin
  if Assigned(FClickItem) then
    FClickItem(Sender);
end;

constructor TFrameListModel.Create(AOwner: TComponent);
begin
  inherited;
  FClickItem := nil;
  FObjList := TObjectList<TControl>.Create;
  FItemSelected := nil;
  FFrameListOnDemand := nil;
  FOnDemandBoolean := nil;
  ItemAlign := TAlignLayout.Top;
end;

destructor TFrameListModel.Destroy;
begin
  if (Assigned(FObjList)) then
  begin
    while FObjList.Count > 0 do
      FObjList.ExtractAt(0);
    FreeAndNil(FObjList);
  end;
  inherited;
end;

procedure TFrameListModel.EndUpdate;
begin
  inherited;
  VtsListEndUpdate;
  vtsList.Visible := True;
end;

function TFrameListModel.GetChildrenCount: Integer;
begin
  Result := vtsList.Content.ChildrenCount;
end;

function TFrameListModel.GetContentHeight: Single;
begin
  Result := 0;

  for var LItem in FObjList do
  begin
    Result := Result + TFrameItemListModel(LItem).Height;
  end;
end;

procedure TFrameListModel.OnPaitingOnDemand(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if Assigned(FFrameListOnDemand) then
  begin // Se estiver assinado tem que remover o Painting
    FFrameListOnDemand.OnPainting := nil;
    FFrameListOnDemand := nil;
  end;
  if Assigned(FOnDemandBoolean) then
    FOnDemandBoolean(False);
end;

procedure TFrameListModel.SetFrameListOnDemand(const Value: TFrame);
begin
  if Assigned(FFrameListOnDemand) then
  begin // Se estiver assinado tem que remover o Painting
    FFrameListOnDemand.OnPainting := nil;
    FFrameListOnDemand := nil;
  end;
  FFrameListOnDemand := Value;
  FFrameListOnDemand.OnPainting := OnPaitingOnDemand;
end;

procedure TFrameListModel.SetItemAlign(const Value: TAlignLayout);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      vtsList.AniCalculations.BoundsAnimation := False;
      vtsList.Parent := lytBackground;
      vtsList.Align := TAlignLayout.Contents;
      vtsList.TabStop := False;
      vtsList.ShowScrollBars := False;
      vtsList.SendToBack;
    end);
  FItemAlign := Value;
end;

procedure TFrameListModel.SetItemSelected(const Value: TControl);
begin
  FItemSelected := Value;
end;

procedure TFrameListModel.SetOnDemandBoolean(const Value: TProc<Boolean>);
begin
  FOnDemandBoolean := Value;
end;

procedure TFrameListModel.BeginUpdate;
begin
  inherited;
  VtsListBeginUpdate;
  vtsList.Visible := False;
end;

procedure TFrameListModel.ClearList;
begin
  try
    VtsListBeginUpdate;
    FObjList.Clear;
  finally
    VtsListEndUpdate;
  end;
end;

procedure TFrameListModel.ClearList(ATarget: TFmxObject;
AClickItem: TNotifyEvent);
begin
  FClickItem := AClickItem;
  FObjList.Clear;

  if Assigned(ATarget) then
    lytBackground.Parent := ATarget;
end;

procedure TFrameListModel.TapItem(Sender: TObject; const Point: TPointF);
begin
  ClickItem(Sender);
end;

procedure TFrameListModel.VtsListBeginUpdate;
begin
  vtsList.BeginUpdate;
  vtsList.Content.BeginUpdate;
end;

procedure TFrameListModel.VtsListEndUpdate;
begin
  vtsList.Content.EndUpdate;
  vtsList.EndUpdate;
end;

end.
