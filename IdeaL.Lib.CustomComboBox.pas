unit IdeaL.Lib.CustomComboBox;

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
  FMX.Effects,
  FMX.Edit,
  FMX.Controls.Presentation;

type
  TItem = class
  private
    FDetail: string;
    FId: string;
    FText: string;
    procedure SetDetail(const Value: string);
    procedure SetId(const Value: string);
    procedure SetText(const Value: string);
    { private declarations }
  protected
    { protected declarations }
  public
    property Id: string read FId write SetId;
    property Text: string read FText write SetText;
    property Detail: string read FDetail write SetDetail;
    { public declarations }
  published
    { published declarations }
  end;

  TItemList = class
  private
    FItems: TObjectList<TItem>;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Items: TObjectList<TItem> read FItems;
    { public declarations }

  published
    { published declarations }
  end;

  TFrmCustomComboBox = class(TFrame)
    rctBackground: TRectangle;
    lytBackground: TLayout;
    lytInside: TLayout;
    rctInsideBackground: TRectangle;
    ShadowEffect1: TShadowEffect;
    lytSearch: TLayout;
    edtSearch: TEdit;
    SearchEditButton1: TSearchEditButton;
    vtsList: TVertScrollBox;
  private
    FItemList: TItemList;
    FShowed: Boolean;
    FObjectList: TObjectList<TObject>;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowStart;
    procedure ShowEnd;

    procedure ClearItems();
    procedure ListItems();
    procedure AddItem(const AId: string = ''; const AText: string = '';
      const ADetail: string = '');
    { Public declarations }
  end;

implementation

uses
  IdeaL.Lib.ItemListaSimplePath,
  IdeaL.Lib.Utils;

{$R *.fmx}
{ TFrmCustomComboBox }

procedure TFrmCustomComboBox.AddItem(const AId, AText, ADetail: string);
var
  LItem: TItem;
  LFil: TItemListaSimplePath;
begin
  LFil := TItemListaSimplePath.Create(nil);

  LItem := TItem.Create;
  FItemList.Items.Add(LItem);

  LItem.Id := AId;
  LItem.Text := AText;
  LItem.Detail := ADetail;
end;

procedure TFrmCustomComboBox.ClearItems;
begin
  try
    vtsList.BeginUpdate;
    TUtils.ClearVtsList(vtsList, TItemListaSimplePath);
  finally
    vtsList.EndUpdate;
  end;
end;

constructor TFrmCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  lytInside.Visible := False;
  edtSearch.TextSettings.Font.Family := 'Verdana';
  edtSearch.TextSettings.Font.Size := 12;
  FShowed := False;
  FObjectList := TObjectList<TObject>.Create;
end;

destructor TFrmCustomComboBox.Destroy;
begin
  FreeAndNil(FItemList);
  FreeAndNil(FObjectList);
  inherited;
end;

procedure TFrmCustomComboBox.ListItems;
var
  i: Integer;
  LHeight: Single;
  LShowItem: Boolean;
  LToSearch: string;
  LFil: TItemListaSimplePath;
begin
  // Pesquisar e deixar isso no formulario padrao de listas.
  for i := Pred(vtsList.Content.ChildrenCount) downto 0 do
  begin
    if (vtsList.Content.Children[i] is TItemListaSimplePath) then
    begin
      (vtsList.Content.Children[i] as TItemListaSimplePath).Parent := nil;
    end;
  end;

  LHeight := 0;

  for i := 0 to Pred(FObjectList.Count) do
  begin
    LShowItem := True;
    LFil := (FObjectList.Items[i] as TItemListaSimplePath);

    LToSearch := edtSearch.Text;
    if not(LToSearch.IsEmpty) then
    begin
      if not(LFil.Text.Contains(LToSearch)) //
        and not(LFil.Detail.Contains(LToSearch)) //
      then
        LShowItem := False;
    end;

    if (LShowItem) then
    begin
      // vtsList.AddObject(LFil);
      LFil.Parent := vtsList;
      LFil.Position.Y := LHeight;
      LHeight := LHeight + LFil.Height + 1;
    end;
  end;
end;

procedure TFrmCustomComboBox.ShowEnd;
begin
  lytInside.BringToFront;
  vtsList.EndUpdate;
  lytInside.Visible := True;
  FShowed := True;
end;

procedure TFrmCustomComboBox.ShowStart;
begin
  FShowed := False;
  edtSearch.Text := EmptyStr;
  lytBackground.BringToFront;
  vtsList.BeginUpdate;
  lytInside.Visible := False;
end;

{ TItem }

procedure TItem.SetDetail(const Value: string);
begin
  FDetail := Value;
end;

procedure TItem.SetId(const Value: string);
begin
  FId := Value;
end;

procedure TItem.SetText(const Value: string);
begin
  FText := Value;
end;

{ TItemList }

{ TItemList }

constructor TItemList.Create(AOwner: TComponent);
begin
  //inherited;
  FItems := TObjectList<TItem>.Create;
end;

destructor TItemList.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

end.
