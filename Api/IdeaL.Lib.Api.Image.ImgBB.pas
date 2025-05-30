unit IdeaL.Lib.Api.Image.ImgBB;

(*
  Main website https://pt.imgbb.com/
  API Doc https://api.imgbb.com/
  It's necessary to create an account.

  Read the API Doc, it's self-explained
*)

interface

uses
  IdeaL.Lib.Api,

  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.JSON,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.Net.Mime;

const
  CUrlApi = 'https://api.imgbb.com/1/upload';

type
  TFileType = (ftLocalFile, ftUrl, ftBase64);
  IHTTPResponse = System.Net.HttpClient.IHTTPResponse;

  TImageJsonResponse = record
  private
    FStatusText: string;
    FStatusCode: Integer;
    FJson: string;
    FId: string;
    FUrlDelete: string;
    FUrlMedium: string; // Medium size file URL
    FUrlThumb: string; // Thumb file URL
    FUrlImage: string; // Original file URL
    FTitle: string; // File's title
    FSize: Integer; // Original file's size
    FMime: string; // File's type

    procedure LoadFromJson(const AJson: string);
  public
    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property JSON: string read FJson;

    property Id: string read FId;
    property Title: string read FTitle;
    property Size: Integer read FSize;
    property Mime: string read FMime;
    property UrlDelete: string read FUrlDelete;
    property UrlImage: string read FUrlImage;
    property UrlMedium: string read FUrlMedium;
    property UrlThumb: string read FUrlThumb;

    procedure LoadFromHTTPResponse(const AResponse: IHTTPResponse);
  end;

  TImgBBApi = class(TIdeaLApi)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    class function Upload(
      const AApiKey, ABody: string;
      const AFileType: TFileType = ftBase64;
      AOnSendDataEvent: TSendDataEvent = nil
      ): TImageJsonResponse;
    class function Download(const AUrl: string; out AResponseContent: TStream; AOnReceiveData: TReceiveDataEvent = nil): Boolean; overload;
    class function Download(const AUrl, AFilePath: string; AOnReceiveData: TReceiveDataEvent = nil): Boolean; overload;
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TImgBBApi }

class function TImgBBApi.Download(
  const AUrl: string;
  out AResponseContent: TStream;
  AOnReceiveData: TReceiveDataEvent): Boolean;
var
  LNetHttpClient: TNetHTTPClient;
  LNetHttpRequest: TNetHTTPRequest;
  LResponse: IHTTPResponse;
begin
  GetNetHttpRequest(LNetHttpClient, LNetHttpRequest, nil, AOnReceiveData);
  AResponseContent := TMemoryStream.Create;
  try
    try
      LResponse := LNetHttpRequest.Get(AUrl, AResponseContent, nil);
      if LResponse.StatusCode <> 200 then
        raise Exception.Create(LResponse.StatusText);
      Result := True;
    except
      // Free, just if something went wrong
      FreeAndNil(AResponseContent);
      raise ;
    end;
  finally
    FreeAndNil(LNetHttpRequest);
    FreeAndNil(LNetHttpClient);
  end;
end;

class function TImgBBApi.Download(
  const AUrl, AFilePath: string;
  AOnReceiveData: TReceiveDataEvent): Boolean;
var
  LResponseContent: TStream;
  LPath: string;
begin
  LResponseContent := nil;
  if Download(AUrl, LResponseContent, AOnReceiveData) then
  begin
    try
      LPath := ExtractFilePath(AFilePath);
      if not DirectoryExists(LPath) then
        ForceDirectories(LPath);
      if not DirectoryExists(LPath) then
        raise Exception.Create('Couldn''t create the path: ' + LPath);
      TMemoryStream(LResponseContent).SaveToFile(AFilePath);
      if not FileExists(AFilePath) then
        raise Exception.Create('Couldn''t save the file on: ' + AFilePath);
    finally
      FreeAndNil(LResponseContent);
    end;
  end;
end;

class function TImgBBApi.Upload(
  const AApiKey, ABody: string;
  const AFileType: TFileType = ftBase64;
  AOnSendDataEvent: TSendDataEvent = nil): TImageJsonResponse;
var
  LHttp: THTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
begin
  LUrl := Format('%s?key=%s', [CUrlApi, AApiKey]);
  LHttp := THTTPClient.Create;
  var
  LSource := TMultipartFormData.Create;
  try
    case AFileType of
      ftLocalFile:
        begin
          LSource.AddFile('image', ABody);
        end;
      ftUrl, ftBase64:
        LSource.AddField('image', ABody);
    end;
    LResponse := LHttp.Post(LUrl, LSource, nil, nil);
  finally
    LSource.Free;
    FreeAndNil(LHttp);
  end;

  if LResponse.StatusCode <> 200 then
  begin
    var
    LMsg := EmptyStr;
    try
      LMsg := LResponse.ContentAsString(TEncoding.UTF8);
      if not LMsg.Trim.IsEmpty then
        LMsg := ' - ' + LMsg;
    except

    end;
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText + LMsg);
  end;
  Result.LoadFromHTTPResponse(LResponse);
end;

{ TImageJsonResponse }

procedure TImageJsonResponse.LoadFromHTTPResponse(
  const AResponse: IHTTPResponse);
begin
  FStatusCode := AResponse.StatusCode;
  FStatusText := AResponse.StatusText;
  FJson := AResponse.ContentAsString;
  LoadFromJson(FJson);
end;

procedure TImageJsonResponse.LoadFromJson(const AJson: string);
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;
  LJsonAux: TJSONValue;
  LJson: string;
  LKey: string;
begin
  try
    LJsonObj := TJSONObject.Create;
    LJsonValue := LJsonObj.ParseJSONValue(AJson);
    LJsonAux := nil;

    if not(Assigned(LJsonValue)) then
      Exit;

    try
      LKey := 'data';
      if LJsonValue.FindValue(LKey) <> nil then
      begin
        LJson := (LJsonValue.FindValue(LKey) as TJSONObject).ToString;

        FreeAndNil(LJsonValue);
        LJsonValue := LJsonObj.ParseJSONValue(LJson);

        LJsonValue.TryGetValue<string>('id', FId);
        LJsonValue.TryGetValue<string>('title', FTitle);
        LJsonValue.TryGetValue<Integer>('size', FSize);
        LJsonValue.TryGetValue<string>('delete_url', FUrlDelete);

        LKey := 'image';
        if LJsonValue.FindValue(LKey) <> nil then
        begin
          LJson := (LJsonValue.FindValue(LKey) as TJSONObject).ToString;

          FreeAndNil(LJsonAux);
          LJsonAux := LJsonObj.ParseJSONValue(LJson);
          LJsonAux.TryGetValue<string>('mime', FMime);
          LJsonAux.TryGetValue<string>('url', FUrlImage);
        end;

        LKey := 'medium';
        if LJsonValue.FindValue(LKey) <> nil then
        begin
          LJson := (LJsonValue.FindValue(LKey) as TJSONObject).ToString;

          FreeAndNil(LJsonAux);
          LJsonAux := LJsonObj.ParseJSONValue(LJson);
          LJsonAux.TryGetValue<string>('url', FUrlMedium);
        end;

        LKey := 'thumb';
        if LJsonValue.FindValue(LKey) <> nil then
        begin
          LJson := (LJsonValue.FindValue(LKey) as TJSONObject).ToString;

          FreeAndNil(LJsonAux);
          LJsonAux := LJsonObj.ParseJSONValue(LJson);
          LJsonAux.TryGetValue<string>('url', FUrlThumb);
        end;
      end;
    except

    end;
  finally
    FreeAndNil(LJsonAux);
    FreeAndNil(LJsonValue);
    FreeAndNil(LJsonObj);
  end;
end;

end.
