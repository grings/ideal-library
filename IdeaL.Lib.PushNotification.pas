unit IdeaL.Lib.PushNotification;

{
  Thanks to those bellow, who helped with testing and provided valuable feedback:
  Temis - temi@pumasistemas.com.br


  * How to implement?
  Sender:
  (Legacy, disabled by Google on 2024.06.20) https://ibb.co/swSHgt0

  Receiver
  - Need: Project->Options->Entitlement List->Receive push notifications
  - Delphi 10.3.3+ http://docwiki.embarcadero.com/RADStudio/Sydney/en/Firebase_Android_Support
  -- To use the example, follow the link until step number 8

  * Permissions:
  For Android13+ it is needed android.permission.POST_NOTIFICATIONS

  ** For Android13+
  Check out this Forum: https://en.delphipraxis.net/topic/8075-android-13-ask-permission-for-push-notification/
  Check out the oficial DOC: https://developer.android.com/develop/ui/views/notifications/notification-permission
}

interface

uses
  System.SysUtils,

  // Receiver
  System.PushNotification,
{$IF (defined(MSWINDOWS) or defined(LINUX))}
  DW.FCMSender,
{$ENDIF}
{$IFDEF ANDROID}
  FMX.PushNotification.Android,
{$ENDIF}
{$IFDEF IOS}
  DW.FCMManager,
{$ENDIF}


{$IF CompilerVersion <= 33.0} // Delphi 10.3.3 or lower

{$IFEND}
  // Receiver

  // Sender
  System.Classes,
  System.Net.HttpClient,
  System.JSON
  // Sender
    ;

type
  TPushNotificationReceiver = class
  private
  var
{$IFDEF IOS}
    FIsStarted: Boolean;
{$ENDIF}
    FPushService: TPushService;
    FOnReceiveNotificationEvent: TProc<string>;
    FOnChangeEventError: TProc<string>;
    FDoDeviceTokenHasBeenTaken: TProc;
    FPushServiceConnection: TPushServiceConnection;
    FDeviceToken: string;
    FDeviceId: string;

    class var FPushNotification: TPushNotificationReceiver;

    constructor Create;

    procedure SetDeviceToken(const Value: string);
    procedure DoChangeEvent(Sender: TObject; AChange: TPushService.TChanges);    
    procedure DoReceiveNotificationEvent(Sender: TObject; const ANotification: TPushServiceNotification);
    function GetStartupNotifications: TArray<string>;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

    class function GetPushNotification: TPushNotificationReceiver; static;
    class function GetServiceCount: Integer; static;
    
    procedure DoStarted(Sender: TObject);
    procedure DoMessageReceivedEvent(Sender: TObject; const JSON: TJSONObject);
    procedure DoTokenReceivedEvent(Sender: TObject; const Token: string);
    { private declarations }
  protected
    { protected declarations }
  public
    destructor Destroy; override;

    class property Instance: TPushNotificationReceiver read GetPushNotification;
    class property ServiceCount: Integer read GetServiceCount;
    property Active: Boolean read GetActive write SetActive;

    property DeviceToken: string read FDeviceToken;
    property DeviceId: string read FDeviceId;
    property OnReceiveNotificationEvent: TProc<string> read FOnReceiveNotificationEvent write FOnReceiveNotificationEvent;
    property OnDeviceTokenHasBeenTaken: TProc read FDoDeviceTokenHasBeenTaken write FDoDeviceTokenHasBeenTaken;
    property OnChangeEventError: TProc<string> read FOnChangeEventError write FOnChangeEventError;

    property StartupNotifications: TArray<string> read GetStartupNotifications;

    function StartupNotificationsStr: string;
    { public declarations }
  end;

{$IF defined(MSWINDOWS) or defined(LINUX)}
  TPushNotificationSender = class
  private
    const
    // FUrlGcm = 'https://android.googleapis.com/gcm/send';
    FUrlFcm = 'https://fcm.googleapis.com/fcm/send';

  var
    FSenderId: string;
    FApiKey: string;
    class var FPushNotification: TPushNotificationSender;

    constructor Create;
    class function GetPushNotification: TPushNotificationSender; static;
    { private declarations }
  protected
    { protected declarations }
  public
    property SenderId: string read FSenderId write FSenderId;
    property ApiKey: string read FApiKey write FApiKey;

    class property Instance: TPushNotificationSender read GetPushNotification;

    // It DOESNT show the Notification Icon on the Device
    function Send(
      ATokens: TArray<string>;
      AFields: TArray<string>;
      AValues: TArray<string>;
      const AProcDataBeferoSend: TProc<string> = nil;
      const AProcResponse: TProc<string> = nil
      ): string; overload;
    // It DOES show the Notification Icon on the Device
    function Send(
      const ATitle: string;
      const AId: string;
      const ABody: string;
      ATokens: TArray<string>;
      AFields: TArray<string>;
      AValues: TArray<string>;
      const AProcDataBeferoSend: TProc<string> = nil;
      const AProcResponse: TProc<string> = nil
      ): string; overload;
    { public declarations }
  end;

  TPushNotificationSenderV1 = class
  private
    const
    FUrl = 'https://fcm.googleapis.com/v1/projects/%s/messages:send';

    class var FPushNotification: TPushNotificationSenderV1;

  var
    FJsonPrivateKeyPath: string;
    FJsonStr: string;
    FReference: string;
    FOAuthToken: string;
    FProcErrorOrResponse: TProc<string>;

    constructor Create;
    class function GetPushNotification: TPushNotificationSenderV1; static;
    { private declarations }
  protected
    procedure OnError(Sender: TObject; const Error: TFCMSenderError); virtual;
    procedure OnResponse(Sender: TObject; const Response: TFCMSenderResponse); virtual;
    { protected declarations }
  public
    function JsonPrivateKeyPath(AValue: string): TPushNotificationSenderV1; overload;
    function JsonPrivateKeyPath: string; overload;

    function JsonStr(AValue: string): TPushNotificationSenderV1; overload;
    function JsonStr: string; overload;

    /// <summary> Since the OnResponse or OnError doesn't reply the DeviceToken
    /// use this reference to check on the CallBack which Send did this Response or Error
    /// </summary>
    function Reference(AValue: string): TPushNotificationSenderV1; overload;
    function Reference: string; overload;

    class property Instance: TPushNotificationSenderV1 read GetPushNotification;

    /// <remarks>
    /// <see cref="https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages"/>
    /// </remarks>
    function Send(
      const ATitle: string;
      const AChannelId: string;
      const ABody: string;
      AData: string;
      ATokens: TArray<string>;
      AIsSilent: Boolean = False;
      const AProcResponse: TProc<string> = nil;
      APriority: Integer = 0  //check DW.FCMSender cFCMMessagePriorityValues
      ): TPushNotificationSenderV1; overload;
    { public declarations }
  end;
{$ENDIF}

implementation

uses
  IdeaL.Lib.Utils;

{ TPushNotification }

constructor TPushNotificationReceiver.Create;
var
  AServiceName: string;
begin
  FOnReceiveNotificationEvent := nil;
  FDoDeviceTokenHasBeenTaken := nil;
  FOnChangeEventError := nil;

  FDeviceId := EmptyStr;
  FDeviceToken := EmptyStr;

{$IFDEF IOS}
  FIsStarted := False;
  FCM.ShowBannerIfForeground := True;
  FCM.OnMessageReceived := DoMessageReceivedEvent;
  FCM.OnStarted := DoStarted;
  FCM.OnTokenReceived := DoTokenReceivedEvent;
  FCM.OnNotificationCategory := nil; 

{$ELSE}
{$IFDEF ANDROID}
{$IF CompilerVersion >= 34.0}
  AServiceName := TPushService.TServiceNames.FCM;
{$ELSE}
  AServiceName := TPushService.TServiceNames.GCM;
{$ENDIF}
{$ENDIF}
  FPushService := TPushServiceManager.Instance.GetServiceByName(AServiceName);

  if not Assigned(FPushService) then
    raise Exception.Create('TPushNotification.Create FPushServer nil');

  FPushServiceConnection := TPushServiceConnection.Create(FPushService);
  FPushServiceConnection.OnChange := DoChangeEvent;
  FPushServiceConnection.OnReceiveNotification := DoReceiveNotificationEvent;
  FPushServiceConnection.Active := False;
{$ENDIF}  
end;

destructor TPushNotificationReceiver.Destroy;
begin
  FPushNotification := nil;
  if Assigned(FPushServiceConnection) then
  begin
    FPushServiceConnection.Active := False;
    FPushServiceConnection.OnChange := nil;
    FPushServiceConnection.OnReceiveNotification := nil;
  end;
  FOnReceiveNotificationEvent := nil;
  FDoDeviceTokenHasBeenTaken := nil;
  if (Assigned(FPushService)) and
    (Assigned(TPushServiceManager.Instance)) and
    (TPushServiceManager.Instance.Count > 0) and
    (TPushServiceManager.Instance.IndexOfService(FPushService) >= 0)
  then
  begin
    TPushServiceManager.Instance.RemoveService(FPushService);
  end;
  FreeAndNil(FPushServiceConnection);
  FreeAndNil(FPushService);

  Sleep(500); // Necessary for Delphi11.3 that was raising error on System.PushNotification.TPushServiceManager.IndexOfService
  inherited;
end;

procedure TPushNotificationReceiver.DoChangeEvent(Sender: TObject;
  AChange: TPushService.TChanges);
begin
  if (TPushService.TChange.Status in AChange) then
  begin
    if (FPushService.Status = TPushService.TStatus.StartupError) and (Assigned(FOnChangeEventError)) then
    begin
      FOnChangeEventError('ERROR TPushNotificationReceiver.DoChangeEvent ' + FPushService.StartupError);
      Exit;
    end;
  end;
  if (TPushService.TChange.DeviceToken in AChange) and (FDeviceToken.Trim.IsEmpty) then
    SetDeviceToken(FPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken]);
end;

procedure TPushNotificationReceiver.DoMessageReceivedEvent(Sender: TObject;
  const JSON: TJSONObject);
begin
  TUtils.EventLog('TPushNotificationReceiver.DoMessageReceivedEvent 1');
  if Assigned(FOnReceiveNotificationEvent) then
  begin
{$IFDEF IOS}
    if FIsStarted then
      JSON.AddPair('gcm.notification.android_channel_id', 'TPushNotificationReceiver.DoMessageReceivedEvent');
{$ENDIF}
    FOnReceiveNotificationEvent(JSON.ToString);
  end;
  TUtils.EventLog('TPushNotificationReceiver.DoMessageReceivedEvent 2')
end;

procedure TPushNotificationReceiver.DoReceiveNotificationEvent(Sender: TObject;
  const ANotification: TPushServiceNotification);
begin
  if Assigned(FOnReceiveNotificationEvent) then
    FOnReceiveNotificationEvent(ANotification.JSON.ToString);
end;

procedure TPushNotificationReceiver.DoStarted(Sender: TObject);
begin                                                   
  TUtils.EventLog('TPushNotificationReceiver.DoStarted 1');
{$IFDEF IOS}
  FIsStarted := True;
{$ENDIF}
end;

procedure TPushNotificationReceiver.DoTokenReceivedEvent(Sender: TObject;
  const Token: string);
begin
{$IFDEF IOS}
  FDeviceId := FCM.GetDeviceID;
{$ENDIF}                       
  SetDeviceToken(Token);
end;

function TPushNotificationReceiver.GetActive: Boolean;
begin
  Result := False;
  if Assigned(FPushServiceConnection) then
    Result := FPushServiceConnection.Active;
end;

class function TPushNotificationReceiver.GetPushNotification: TPushNotificationReceiver;
begin
  if not Assigned(FPushNotification) then
  begin
    try
      FPushNotification := TPushNotificationReceiver.Create;
    except
      FreeAndNil(FPushNotification);
      raise ;
    end;
  end;
  Result := FPushNotification;
end;

class function TPushNotificationReceiver.GetServiceCount: Integer;
begin
  Result := TPushServiceManager.Instance.Count;
end;

function TPushNotificationReceiver.GetStartupNotifications: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, 0);

{$IFDEF IOS}
  // It checks Startup Notifications on TCustomPlatformFCMManager.Started;
{$ELSE}
  for i := 0 to Pred(Length(FPushService.StartupNotifications)) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[i] := FPushService.StartupNotifications[i].JSON.ToString
  end;

  // SetLength(FPushService.StartupNotifications, 0);
{$ENDIF}
end;

procedure TPushNotificationReceiver.SetActive(const Value: Boolean);
begin
{$IFDEF IOS}
  if not FCM.IsStarted then  
    FCM.Start;
{$ELSE}
  if Assigned(FPushServiceConnection) then
  begin
    FPushServiceConnection.Active := Value;
    if Value then
    begin
      FDeviceId := FPushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
      FDeviceToken := FPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    end;
  end;
{$ENDIF}  
end;

procedure TPushNotificationReceiver.SetDeviceToken(const Value: string);
begin
  FDeviceToken := Value;
  if Assigned(FDoDeviceTokenHasBeenTaken) then
    FDoDeviceTokenHasBeenTaken;
end;

function TPushNotificationReceiver.StartupNotificationsStr: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to Pred(Length(StartupNotifications)) do
  begin
    Result := Result + #13 + StartupNotifications[i]
  end;
end;

{$IF defined(MSWINDOWS) or defined(LINUX)}
{ TPushNotificationSender }

constructor TPushNotificationSender.Create;
begin
  FSenderId := EmptyStr;
  FApiKey := EmptyStr;
end;

class function TPushNotificationSender.GetPushNotification: TPushNotificationSender;
begin
  if not Assigned(FPushNotification) then
  begin
    try
      FPushNotification := TPushNotificationSender.Create;
    except
      FreeAndNil(FPushNotification);
      raise ;
    end;
  end;
  Result := FPushNotification;
end;

function TPushNotificationSender.Send(
  const ATitle, AId, ABody: string;
  ATokens, AFields, AValues: TArray<string>;
  const AProcDataBeferoSend, AProcResponse: TProc<string>): string;
var
  LHttpClient: THttpClient;
  LData: TStringStream;
  LResponse: TStringStream;
  LJson: TJSONObject;
  LJsonData: TJSONObject;
  LJsonNotification: TJSONObject;
  LJsonTokens: TJSONArray;
  i: Integer;
  LUrl: string;
begin
  Result := EmptyStr;
  LData := nil;
  LResponse := nil;
  LHttpClient := nil;
  LJsonNotification := nil;
  LJsonTokens := nil;
  LJsonData := nil;
  LJson := nil;
  // {$IF CompilerVersion >= 34.0}
  LUrl := FUrlFcm;
  (* {$ELSE}
    LUrl := FUrlGcm;
    {$ENDIF} *)
  if (Length(AFields) = 0) or (Length(AValues) = 0) or (Length(AFields) <> Length(AValues)) then
    raise Exception.Create('TPushNotificationSender.Send Field x Value is wrong');

  try
    LJsonTokens := TJSONArray.Create;
    for i := 0 to Pred(Length(ATokens)) do
    begin
      if (Length(ATokens) = 0) or (ATokens[i].Trim.IsEmpty) then
        raise Exception.Create('TPushNotificationSender.Send DeviceToken can NOT be empty');
      LJsonTokens.Add(ATokens[i]);
    end;

    LJsonData := TJSONObject.Create;
    for i := 0 to Pred(Length(AFields)) do
    begin
      if not AFields[i].Trim.IsEmpty then
        LJsonData.AddPair(AFields[i], AValues[i]);
    end;

    if not ATitle.Trim.IsEmpty and
      not AId.Trim.IsEmpty and
      not ABody.Trim.IsEmpty
    then
    begin
      LJsonNotification := TJSONObject.Create;
      LJsonNotification.AddPair('title', ATitle);
      LJsonNotification.AddPair('message_id', AId);
      LJsonNotification.AddPair('body', ABody);
    end;

    LJson := TJSONObject.Create;
    LJson.AddPair('registration_ids', LJsonTokens);
    LJson.AddPair('data', LJsonData);
    if Assigned(LJsonNotification) then
      LJson.AddPair('notification', LJsonNotification);

    LData := TStringStream.Create(LJson.ToString, TEncoding.UTF8);
    LData.Position := 0;

    if Assigned(AProcDataBeferoSend) then
      AProcDataBeferoSend(LData.DataString);

    LResponse := TStringStream.Create;
    LHttpClient := THttpClient.Create;
    LHttpClient.ContentType := 'application/json';
    LHttpClient.CustomHeaders['Authorization'] := 'key=' + FApiKey;
    LHttpClient.Post(LUrl, LData, LResponse);
    LResponse.Position := 0;

    Result := LResponse.DataString;

    if Assigned(AProcResponse) then
      AProcResponse(LResponse.DataString);
  finally
    FreeAndNil(LHttpClient);
    FreeAndNil(LData);
    FreeAndNil(LResponse);
    FreeAndNil(LJson); // It releases all JSON within
  end;
end;

function TPushNotificationSender.Send(
  ATokens, AFields, AValues: TArray<string>;
  const AProcDataBeferoSend, AProcResponse: TProc<string>): string;
begin
  Result := EmptyStr;
  {
    Why use a hidden notification?
    Message broadcasting!
    You can send a 'secret' message which you APP will receive it and execute
    something when your APP  is open
    E.g.: You user is looking on the APP for his sales results; a remote saler
    just finished a big sell and it was applied in your DB, so your Server will
    send that hidden notification for all Managers, the APP will receive it and
    read the message, it will find the key or some value which correspond to
    update the sale results.
  }

  Result := Send(
    EmptyStr,
    EmptyStr,
    EmptyStr,
    ATokens,
    AFields,
    AValues,
    AProcDataBeferoSend,
    AProcResponse);
end;

{ TPushNotificationSenderV1 }

constructor TPushNotificationSenderV1.Create;
begin
  JsonPrivateKeyPath(EmptyStr);
end;

class function TPushNotificationSenderV1.GetPushNotification: TPushNotificationSenderV1;
begin
  if not Assigned(FPushNotification) then
  begin
    try
      FPushNotification := TPushNotificationSenderV1.Create;
    except
      FreeAndNil(FPushNotification);
      raise ;
    end;
  end;
  Result := FPushNotification;
end;


function TPushNotificationSenderV1.JsonPrivateKeyPath: string;
begin
  Result := FJsonPrivateKeyPath.Trim;
end;

function TPushNotificationSenderV1.JsonStr: string;
begin
  Result := FJsonStr.Trim;
end;

function TPushNotificationSenderV1.JsonStr(
  AValue: string): TPushNotificationSenderV1;
begin
  Result := Self;
  FJsonStr := AValue;
end;

function TPushNotificationSenderV1.Reference(AValue: string): TPushNotificationSenderV1;
begin
  Result := Self;
  FReference := AValue;
end;

function TPushNotificationSenderV1.Reference: string;
begin
  Result := FReference;
end;

function TPushNotificationSenderV1.JsonPrivateKeyPath(
  AValue: string): TPushNotificationSenderV1;
begin
  Result := Self;
  FJsonPrivateKeyPath := AValue;
end;

procedure TPushNotificationSenderV1.OnError(Sender: TObject;
  const Error: TFCMSenderError);
begin
  if Assigned(FProcErrorOrResponse) then
  begin
    var
    LJSON := TJSONObject.Create;
    try
      if not FReference.Trim.IsEmpty then
        LJSON.AddPair('Reference', FReference);

      LJSON.AddPair('Content', Error.Content);
      LJSON.AddPair('ErrorMessage', Error.ErrorMessage);
      LJSON.AddPair('TFCMSenderErrorKind', TUtils.GetEnumName<TFCMSenderErrorKind>(Error.Kind));
      FProcErrorOrResponse(LJSON.ToJSON);
    finally
      LJSON.Free;
    end;
  end;
end;

procedure TPushNotificationSenderV1.OnResponse(Sender: TObject;
  const Response: TFCMSenderResponse);
begin
  if Assigned(FProcErrorOrResponse) then
  begin
    var
    LJSON := TJSONObject.Create;
    try
      if not FReference.Trim.IsEmpty then
        LJSON.AddPair('Reference', FReference);

      LJSON.AddPair('Response', Response.Response);
      FProcErrorOrResponse(LJSON.ToJSON);
    finally
      LJSON.Free;
    end;
  end;
end;

function TPushNotificationSenderV1.Send(const ATitle, AChannelId, ABody: string;
  AData: string; ATokens: TArray<string>; AIsSilent: Boolean;
  const AProcResponse: TProc<string>; APriority: Integer): TPushNotificationSenderV1;
var
  i: Integer;
begin
  Result := Self;

  if (Length(ATokens) = 0) then
    raise Exception.Create('TPushNotificationSenderV1.Send DeviceToken can NOT be empty');

  if AIsSilent then
  begin
    if AData.Trim.IsEmpty then
      raise Exception.Create('Data can not be empty while sending Silent notification');
  end
  else
  begin
    if (ATitle.Trim.IsEmpty) or
      (ABody.Trim.IsEmpty)
    then
      raise Exception.Create('Tile and Body can not be empty while sending Noisy notification');
  end;

  if not AData.Trim.IsEmpty then
  begin
    var
    LJsonData := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AData), 0) as TJSONObject;
    try
      if not Assigned(LJsonData) then
        raise Exception.Create('TPushNotificationSenderV1.Send Invalid JSON Object String for DATA param: ' + AData);
      if AIsSilent then
      begin
        var
        LFound := LJsonData.FindValue('isSilent');
        if LFound <> nil then
          LJsonData.RemovePair('isSilent').Free;
        LJsonData.AddPair('isSilent', TJSONString.Create('1'));
        LJsonData.AddPair('title', TJSONString.Create(ATitle));
        LJsonData.AddPair('body', TJSONString.Create(ABody));
        if not AChannelId.Trim.IsEmpty then
          LJsonData.AddPair('channel_id', TJSONString.Create(AChannelId));
      end;
      AData := LJsonData.ToString;
    finally
      LJsonData.Free;
    end;
  end;

  FProcErrorOrResponse := AProcResponse;
  var
  LFCMSender := TFCMSender.Create;
  try
    LFCMSender.OnError := OnError;
    LFCMSender.OnResponse := OnResponse;

    if (not (JsonStr.IsEmpty)) then
      LFCMSender.ServiceAccount.Parse(JsonStr)
    else
      LFCMSender.LoadServiceAccount(JsonPrivateKeyPath);
    var
    LMessage := TFCMMessage.Create;
    try
      if not AIsSilent then
      begin
        LMessage.Title := ATitle;
        LMessage.Body := ABody;
        if not AChannelId.Trim.IsEmpty then
          LMessage.ChannelID := AChannelId;
      end;
      //LMessage.IsDataOnly := True;
      LMessage.Data := AData;
      LMessage.Options := LMessage.Options - [TFCMMessageOption.ContentAvailable];
      LMessage.Priority := TFCMMessagePriority(APriority);
      if not AIsSilent then
        LMessage.Options := LMessage.Options + [TFCMMessageOption.ContentAvailable];
      for i := 0 to Pred(Length(ATokens)) do
      begin
        LFCMSender.Post(LMessage.GetTokenPayload(ATokens[i]));
      end;
    finally
      LMessage.Free;
    end;
  finally
    LFCMSender.Free;
  end;
end;
{$ENDIF}

initialization

TPushNotificationReceiver.FPushNotification := nil;
{$IFDEF MSWINDOWS}
TPushNotificationSender.FPushNotification := nil;
TPushNotificationSenderV1.FPushNotification := nil;
{$ENDIF}

finalization

FreeAndNil(TPushNotificationReceiver.FPushNotification);
{$IFDEF MSWINDOWS}
FreeAndNil(TPushNotificationSender.FPushNotification);
FreeAndNil(TPushNotificationSenderV1.FPushNotification);
{$ENDIF}

end.
