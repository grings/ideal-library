unit IdeaL.Lib.Model.DropBox.ListFolder;

interface

uses
  IdeaL.Lib.CustomRtti,
  IdeaL.Lib.Model.DropBox.ListFolder.Entries, System.SysUtils;

type

  [TLocalObject('DropBoxListFolder', ddbNone)]
  TDropBoxListFolder = class(TCustomTable)
  private
    [TJsonField('cursor', mftString)]
    FCursor: string;
    [TJsonField('has_more', mftBooleanStr)]
    FHasMore: Boolean;
    [TJsonField('entries', mftObjectList)]
    FEntries: TDropBoxListFolderEntriesList;
    procedure SetEntries(const Value: TDropBoxListFolderEntriesList);
    procedure SetCursor(const Value: string);
    procedure SetHasMore(const Value: Boolean);
    { private declarations }
  public
    constructor Create; override;
    destructor Destroy; override;

    property Cursor: string read FCursor write SetCursor;
    property HasMore: Boolean read FHasMore write SetHasMore;
    property Entries: TDropBoxListFolderEntriesList read FEntries write SetEntries;
    { public declarations }
  published
    { published declarations }
  end;

  [TLocalObject('DropBoxListFolder', ddbNone)]
  TDropBoxListFolderList = class(TCustomTableList<TDropBoxListFolder>)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TDropBoxListFolder }

constructor TDropBoxListFolder.Create;
begin
  inherited;
  FEntries := TDropBoxListFolderEntriesList.Create;
end;

destructor TDropBoxListFolder.Destroy;
begin
  FreeAndNil(FEntries);
  inherited;
end;

procedure TDropBoxListFolder.SetCursor(const Value: string);
begin
  FCursor := Value;
end;

procedure TDropBoxListFolder.SetEntries(
  const Value: TDropBoxListFolderEntriesList);
begin
  FEntries := Value;
end;

procedure TDropBoxListFolder.SetHasMore(const Value: Boolean);
begin
  FHasMore := Value;
end;

end.
