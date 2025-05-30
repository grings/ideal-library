unit IdeaL.Lib.Model.DropBox.ListFolder.Entries;

interface

uses
  IdeaL.Lib.CustomRtti;

type

  [TLocalObject('DropBoxListFolderEntries', ddbNone)]
  TDropBoxListFolderEntries = class(TCustomTable)
  private
    [TJsonField('.tag', mftString)]
    FTag: string;
    [TJsonField('name', mftString)]
    FName: string;
    [TJsonField('path_lower', mftString)]
    FPathLower: string;
    [TJsonField('path_display', mftString)]
    FPathDisplay: string;
    [TJsonField('id', mftString)]
    FId: string;
    [TJsonField('client_modified', mftDateTime)]
    FClientModified: TDateTime;
    [TJsonField('server_modified', mftDateTime)]
    FServerModified: TDateTime;
    [TJsonField('rev', mftString)]
    FRev: string;
    [TJsonField('size', mftDouble)]
    FSize: Double;
    [TJsonField('is_downloadable', mftBooleanStr)]
    FIsDownloadable: Boolean;
    [TJsonField('content_hash', mftString)]
    FContentHash: string;

    procedure SetClientModified(const Value: TDateTime);
    procedure SetContentHash(const Value: string);
    procedure SetId(const Value: string);
    procedure SetIsDownloadable(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetPathDisplay(const Value: string);
    procedure SetPathLower(const Value: string);
    procedure SetRev(const Value: string);
    procedure SetServerModified(const Value: TDateTime);
    procedure SetSize(const Value: Double);
    procedure SetTag(const Value: string);
    {
    FDataHora: TDateTime;
    [TJsonField('Json', mftString)]
    FJson: string;

    procedure SetDataHora(const Value: TDateTime);
    procedure SetJson(const Value: string); }
    { private declarations
      protected
      { protected declarations }
  public
    constructor Create; override;

    property Tag: string read FTag write SetTag;
    property Name: string read FName write SetName;
    property PathLower: string read FPathLower write SetPathLower;
    property PathDisplay: string read FPathDisplay write SetPathDisplay;
    property Id: string read FId write SetId;
    property ClientModified: TDateTime read FClientModified write SetClientModified;
    property ServerModified: TDateTime read FServerModified write SetServerModified;
    property Rev: string read FRev write SetRev;
    property Size: Double read FSize write SetSize;
    property IsDownloadable: Boolean read FIsDownloadable write SetIsDownloadable;
    property ContentHash: string read FContentHash write SetContentHash;

    { public declarations }
  published
    { published declarations }
  end;

  [TLocalObject('DropBoxListFolderEntries', ddbNone)]
  TDropBoxListFolderEntriesList = class(TCustomTableList<TDropBoxListFolderEntries>)
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

{ TDropBoxListFolderEntries }

constructor TDropBoxListFolderEntries.Create;
begin
  inherited;

end;

procedure TDropBoxListFolderEntries.SetClientModified(const Value: TDateTime);
begin
  FClientModified := Value;
end;

procedure TDropBoxListFolderEntries.SetContentHash(const Value: string);
begin
  FContentHash := Value;
end;

procedure TDropBoxListFolderEntries.SetId(const Value: string);
begin
  FId := Value;
end;

procedure TDropBoxListFolderEntries.SetIsDownloadable(const Value: Boolean);
begin
  FIsDownloadable := Value;
end;

procedure TDropBoxListFolderEntries.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDropBoxListFolderEntries.SetPathDisplay(const Value: string);
begin
  FPathDisplay := Value;
end;

procedure TDropBoxListFolderEntries.SetPathLower(const Value: string);
begin
  FPathLower := Value;
end;

procedure TDropBoxListFolderEntries.SetRev(const Value: string);
begin
  FRev := Value;
end;

procedure TDropBoxListFolderEntries.SetServerModified(const Value: TDateTime);
begin
  FServerModified := Value;
end;

procedure TDropBoxListFolderEntries.SetSize(const Value: Double);
begin
  FSize := Value;
end;

procedure TDropBoxListFolderEntries.SetTag(const Value: string);
begin
  FTag := Value;
end;

end.
