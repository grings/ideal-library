unit IdeaL.Lib.CustomRtti;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.StrUtils,
  System.Json,
  System.Rtti,
  System.Variants,
  System.TypInfo,
  System.Math,

  IdeaL.Lib.Controller.IDataSet,

  Data.DB;

type
  IDataSet = IdeaL.Lib.Controller.IDataSet.IDataSet;
  TCustomRttiSettings = class
  private const
    FFormatMySqlDateCosnt = '%d/%m/%Y';
    class var FFormatMySqlDate: string;
    class var FFormatSqLiteDateTime: string;
    class procedure SetFormatSqLiteDateTime(const Value: string); static;
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  public
    // class var FormatMySqlDate: string;
    class property FormatMySqlDate: string read FFormatMySqlDate
      write FFormatMySqlDate;
    class property FormatSqLiteDateTime: string read FFormatSqLiteDateTime
      write SetFormatSqLiteDateTime;
    { public declarations }
  end;

  TFieldType = (ftLocal, ftJson);

  TDriverDatabase = (ddbNone, ddbSqlite, ddbFirebird3_0, ddbMySql8);

  TMyFieldType = (mftString, mftInteger, mftInt64, mftDouble, mftDate,
    mftDateTime, mftBooleanStr, mftObjectList,
    { TODO 1 -oIgorBastos  -cInformation: mftExtendedInt utilizada para receber
      os dados inteiros de tamanhos muito grandes do BD, tipo o DECIMAL(20) do MySQL }
    mftExtendedInt);

  TCustomField = class(TCustomAttribute)
  private
    FFieldName: string;
    FFieldType: TMyFieldType;
    FValueToReturnNull: string;
  public
    constructor Create(AFieldName: string; AFieldType: TMyFieldType = mftString;
      AValueToReturnNull: string = ''); virtual;
    property FieldName: string read FFieldName;
    property FieldType: TMyFieldType read FFieldType;
    property ValueToReturnNull: string read FValueToReturnNull;
  end;

  TJsonField = class(TCustomField)
  private
  public
  end;

  TLocalField = class(TCustomField)
  private
    FDoPost: Boolean;
    FIsPrimary: Boolean;
    FIsAutoInc: Boolean;
    FDoMatch: Boolean;
    FIsToSql: Boolean;

  public
    constructor Create(AFieldName: string; AFieldType: TMyFieldType = mftString;
      const ADoPost: Boolean = True; const AIsPrimary: Boolean = False;
      const AIsAutoInc: Boolean = False; const ADoMatch: Boolean = False;
      const AIsToSql: Boolean = True; AValueToReturnNull: string = '');
      reintroduce; overload;

    property DoPost: Boolean read FDoPost;
    property IsPrimary: Boolean read FIsPrimary;
    property IsAutoInc: Boolean read FIsAutoInc;
    property DoMatch: Boolean read FDoMatch;
    property IsToSql: Boolean read FIsToSql;

  end;

  TLocalObject = class(TCustomAttribute)
  private
    { private declarations }
    FObjectName: string;
    FDriverDatabase: TDriverDatabase;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AObjectName: string; ADriverDatabase: TDriverDatabase);
    property ObjectName: string read FObjectName;
  end;

  TCustomTable = class
  private
    FDataSet: IDataSet;
    procedure SetDataSet(const Value: IDataSet);
    { private declarations }
  protected
    procedure SetFieldValue(ALocalField: TLocalField; ARttiField: TRttiField;
      AFieldValue: TValue); overload;
    procedure DeleteDbItem<T: TCustomTable>(const ASqlWhere: string);
    procedure ApplyUpdatesRtti<T: TCustomTable>(const AApplySubItems
      : Boolean = True);
    { protected declarations }
  public
    constructor Create(); overload; virtual;
    constructor Create(ADataSet: IDataSet); overload; virtual;

    property DataSet: IDataSet read FDataSet write SetDataSet;
    class function GetSql<T: TCustomTable>(const ASqlWhere: string = ''): string;
    function GetField<T: TCustomField>(const AFieldName: string; out AField: T;
      const AShowException: Boolean = True): TRttiField;
    function GetFieldValue(const AFieldName: string;
      const AShowException: Boolean = True;
      const AFieldType: TFieldType = ftLocal): Variant;
    function GetFieldObject(const AFieldName: string;
      const AShowException: Boolean = True;
      const AFieldType: TFieldType = ftLocal): TObject;
    procedure SetFieldValue<T: TCustomField>(const AFieldName: string;
      AFieldValue: TValue; const AShowException: Boolean = True); overload;

    procedure LoadFromJson(const AJson: String); virtual;

    function ToJson(const AJsonFromSubItems: Boolean = True): string; virtual;
    function PropertyExists(const AName: string): Boolean;
    { public declarations }
  end;

  TCustomTableList<T: TCustomTable> = class
  private
    { private declarations }
  var
    FObjectList: TObjectList<T>;
    FDataSet: IDataSet;

    procedure SetObjectList(const Value: TObjectList<T>);
    procedure SetDataSet(const Value: IDataSet);

    procedure ApplyUpdateSqlite(const AObjectName: string;
      const AApplySubItems: Boolean = True);
    procedure ApplyUpdateFirebird3_0(const AObjectName: string;
      const AApplySubItems: Boolean = True);
    procedure ApplyUpdateMySql8(const AObjectName: string;
      const AApplySubItems: Boolean = True);

    function ToJsonFromSqlSqlite(const AObjectName, ASqlWhere: string): string;
    function ToJsonFromSqlFirebird3_0(const AObjectName,
      ASqlWhere: string): string;
    function ToJsonFromSqlMySql8(const AObjectName, ASqlWhere: string): string;
  protected
    { protected declarations }
  public
    { public declarations }
  var
    ClassTypeVar: T;
    constructor Create(); overload; virtual;
    constructor Create(ADataSet: IDataSet); reintroduce; overload; virtual;
    destructor Destroy; override;

    property DataSet: IDataSet read FDataSet write SetDataSet;

    procedure Add(AObject: T); virtual;
    function Item(const AIndex: Integer): T;
    function Count(): Integer;
    procedure Delete(const AIndex: Integer);
    procedure Clear();
    function IsEmpty(): Boolean;
    function First(): T;
    function Last(): T;

    procedure ApplyUpdates(const AApplySubItems: Boolean = True); virtual;
    procedure LoadFromJson(const AJson: String); overload; virtual;
    procedure LoadFromSql(const ASql: string); virtual;
    procedure SortBy(const AFieldName, ATypeOrder: string;
      const AFieldType: TFieldType = ftLocal);
    procedure SortByJson(const AFieldName, ATypeOrder: string);
	function LocateValue(const AFieldName: string; AValue: Variant): T; virtual;

    procedure SaveJsonToFile(const AFile: string;
      const AJsonFromSubItems: Boolean = True);

    function ToJson(const AJsonFromSubItems: Boolean = True): string; virtual;
    function ToJsonFromSql(const ASqlWhere: string = ''): string; virtual;

    property ObjectList: TObjectList<T> read FObjectList write SetObjectList;
  end;

implementation

uses
  IdeaL.Lib.Utils;

{ TCustomField }

constructor TCustomField.Create(AFieldName: string; AFieldType: TMyFieldType;
  AValueToReturnNull: string);
begin
  FFieldName := AFieldName;
  FFieldType := AFieldType;
  FValueToReturnNull := AValueToReturnNull;
end;

{ TLocalField }

constructor TLocalField.Create(AFieldName: string; AFieldType: TMyFieldType;
  const ADoPost, AIsPrimary, AIsAutoInc, ADoMatch, AIsToSql: Boolean;
  AValueToReturnNull: string);
begin
  FFieldName := AFieldName;
  FFieldType := AFieldType;
  FDoPost := ADoPost;
  FIsPrimary := AIsPrimary;
  FIsAutoInc := AIsAutoInc;
  FDoMatch := ADoMatch;
  FIsToSql := AIsToSql;
  FValueToReturnNull := AValueToReturnNull;
end;

{ TLocalObject }

constructor TLocalObject.Create(AObjectName: string;
  ADriverDatabase: TDriverDatabase);
begin
  FObjectName := AObjectName;
  FDriverDatabase := ADriverDatabase;
end;

{ TCustomTable }

procedure TCustomTable.ApplyUpdatesRtti<T>(const AApplySubItems: Boolean);
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LLocalObject: TLocalObject;
  LObjectList: TObject;
  LMyField: TLocalField;
  LObjectName: string;
  LSqlInsert: string;
  LFieldName: string;
  LFieldValue: string;
  LLocalFieldValue: string;
  LFieldValueConcat: string;
  LText: string;
  LDateTime: TDateTime;

  LStrList: TStringList;
begin
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  LRttiAttribute := LRttitype.GetAttributes;
  for X := Low(LRttiAttribute) to High(LRttiAttribute) do
  begin
    if LRttiAttribute[X].ClassNameIs('TLocalObject') then
    begin
      LLocalObject := LRttiAttribute[X] as TLocalObject;
      if Assigned(LLocalObject) then
      begin
        LObjectName := LLocalObject.ObjectName;
        Break;
      end;
    end;
  end;

  if not(LObjectName.IsEmpty) then
  begin
    LSqlInsert := 'insert or replace into ' + LObjectName + ' (%s) values %s';
    LFieldValueConcat := EmptyStr;

    LRttitype := LRttiContext.GetType(Self.ClassType);
    LRttiField := LRttitype.GetFields;
    LFieldName := EmptyStr;
    LFieldValue := EmptyStr;
    for I := Low(LRttiField) to High(LRttiField) do
    begin
      LRttiAttribute := LRttiField[I].GetAttributes;
      for X := Low(LRttiAttribute) to High(LRttiAttribute) do
      begin
        if LRttiAttribute[X].ClassNameIs('TLocalField') then
        begin
          LMyField := LRttiAttribute[X] as TLocalField;
          if (Assigned(LMyField)) then
          begin
            if (LMyField.DoPost) and not(LMyField.IsAutoInc) then
            begin
              if (LMyField.FieldType <> TMyFieldType.mftObjectList) then
              begin
                LFieldName := LFieldName + ',' + LMyField.FieldName;

                case LMyField.FieldType of
                  mftString, mftBooleanStr, mftExtendedInt:
                    LLocalFieldValue :=
                      QuotedStr(VarToStr
                      (Self.GetFieldValue(LMyField.FieldName)));
                  mftInteger, mftInt64:
                    LLocalFieldValue :=
                      VarToStr(Self.GetFieldValue(LMyField.FieldName));
                  mftDouble:
                    LLocalFieldValue :=
                      StringReplace
                      (VarToStr(Self.GetFieldValue(LMyField.FieldName)), ',',
                      '.', [rfReplaceAll]);
                  mftDate:
                  begin
                    TUtils.TryStrToDateTime(
                        VarToStr(Self.GetFieldValue(LMyField.FieldName)),
                        LDateTime
                        );
                    LLocalFieldValue :=
                    '"' + FormatDateTime('yyyy-mm-dd', LDateTime) + '"';
                  end;
                  mftDateTime:
                    begin
                      TUtils.TryStrToDateTime(
                        VarToStr(Self.GetFieldValue(LMyField.FieldName)),
                        LDateTime
                        );
                      LLocalFieldValue := '"' +
                        FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
                        LDateTime
                        ) + '"';
                    end;
                end;

                LFieldValue := LFieldValue + ',' + LLocalFieldValue;
              end
              else if (AApplySubItems) then
              begin
                // raise Exception.Create('Falta implementar o ApplySubItems');
                LObjectList := Self.GetFieldObject(LMyField.FieldName);
                if (Assigned(LObjectList)) and (LObjectList <> nil) then
                begin
                  TCustomTableList<TCustomTable>(LObjectList).ApplyUpdates;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    LFieldValueConcat := LFieldValueConcat + '(' +
      LFieldValue.Trim([',']) + '),';

    LSqlInsert := Format(LSqlInsert, [LFieldName.Trim([',']),
      LFieldValueConcat.Trim([','])]);

    try
      LStrList := TStringList.Create;
      LStrList.Text := StringReplace(LSqlInsert, QuotedStr('null'), 'Null',
        [rfReplaceAll]);
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
      try
        LStrList.SaveToFile('Script' + Self.ClassName +
          '.ApplyUpdatesRtti.sql');
      except

      end;
{$ENDIF}
{$ENDIF}
      DataSet.QryExec(LStrList.Text);
    finally
      LStrList.DisposeOf;
    end;
  end;
end;

constructor TCustomTable.Create(ADataSet: IDataSet);
begin
  Create;
  FDataSet := ADataSet;
end;

constructor TCustomTable.Create;
begin

end;

procedure TCustomTable.DeleteDbItem<T>(const ASqlWhere: string);
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiAttribute: TArray<TCustomAttribute>;
  X: Integer;
  LLocalObject: TLocalObject;
  LObjectName: string;
begin
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  LRttiAttribute := LRttitype.GetAttributes;
  for X := Low(LRttiAttribute) to High(LRttiAttribute) do
  begin
    if LRttiAttribute[X].ClassNameIs('TLocalObject') then
    begin
      LLocalObject := LRttiAttribute[X] as TLocalObject;
      if Assigned(LLocalObject) then
      begin
        LObjectName := LLocalObject.ObjectName;
        Break;
      end;
    end;
  end;

  if (LObjectName.Trim.IsEmpty) then
    Exit;

  if (ASqlWhere.Trim.IsEmpty) then
    raise Exception.Create('Cl�usula Where � obrigat�ria');

  DataSet.ConnExec('delete from ' + LObjectName + ' ' + ASqlWhere);
end;

function TCustomTable.GetField<T>(const AFieldName: string; out AField: T;
  const AShowException: Boolean): TRttiField;
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  I: Integer;
  X: Integer;
  LField: T;
  LText: string;
begin
  // percorro os atributos para preencher corretamente
  Result := nil;

  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  LRttiField := LRttitype.GetFields;
  for I := Low(LRttiField) to High(LRttiField) do
  begin
    LRttiAttribute := LRttiField[I].GetAttributes;

    for X := Low(LRttiAttribute) to High(LRttiAttribute) do
    begin
      { LText := T.ClassName;
        LText := LRttiAttribute[X].ClassName;
        LText := AFieldName.Trim.ToLower;
        if (LText = 'datanascimento') then
        StrToInt('1'); }
      if LRttiAttribute[X].ClassNameIs(T.ClassName) then
      begin
        LField := LRttiAttribute[X] as T;

        if (Assigned(LField)) and
          (LField.FieldName.Trim.ToLower = AFieldName.Trim.ToLower) then
        begin
          AField := LField;
          Result := LRttiField[I];
          Break;
        end;
      end;
      if (Result <> nil) then
        Break;
    end;
    if (Result <> nil) then
      Break;
  end;
  if (Result = nil) and (AShowException) then
    raise Exception.Create('Field [' + AFieldName + '] not found.');
end;

function TCustomTable.GetFieldObject(const AFieldName: string;
  const AShowException: Boolean; const AFieldType: TFieldType): TObject;
var
  LLocalField: TLocalField;
  LJsonField: TJsonField;
begin
  Result := nil;
  case AFieldType of
    ftLocal:
      Result := GetField<TLocalField>(AFieldName, LLocalField, AShowException)
        .GetValue(Self).AsObject;
    ftJson:
      Result := GetField<TJsonField>(AFieldName, LJsonField, AShowException)
        .GetValue(Self).AsObject;
  end;
end;

function TCustomTable.GetFieldValue(const AFieldName: string;
  const AShowException: Boolean; const AFieldType: TFieldType): Variant;
var
  LLocalField: TLocalField;
  LJsonField: TJsonField;
  LRttiField: TRttiField;
  LValue: TValue;
  LStr: string;
begin
  if (AFieldName.Equals('HoraInicio')) then
    StrToInt('1');
  case AFieldType of
    ftLocal:
      Result := GetField<TLocalField>(AFieldName, LLocalField, AShowException)
        .GetValue(Self).AsVariant;
    ftJson:
      begin
        Result := GetField<TJsonField>(AFieldName, LJsonField, AShowException)
          .GetValue(Self).AsVariant;
      end;
  end;

end;

class function TCustomTable.GetSql<T>(const ASqlWhere: string): string;
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LLocalObject: TLocalObject;
  LMyField: TLocalField;
  LObjectName: string;
  LSqlInsert: string;
  LFieldLocal: string;
  LFieldName: string;
  LMyFieldFieldName: string;
  LReescreveAs: Boolean;

  LStrList: TStringList;
begin
  Result := EmptyStr;

  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(T);

  LRttiAttribute := LRttitype.GetAttributes;
  for X := Low(LRttiAttribute) to High(LRttiAttribute) do
  begin
    if LRttiAttribute[X].ClassNameIs('TLocalObject') then
    begin
      LLocalObject := LRttiAttribute[X] as TLocalObject;
      if Assigned(LLocalObject) then
      begin
        LObjectName := LLocalObject.ObjectName;
        if (LLocalObject.FDriverDatabase = TDriverDatabase.ddbMySql8) then
          LObjectName := LObjectName;
        Break;
      end;
    end;
  end;

  if not(LObjectName.IsEmpty) then
  begin
    LSqlInsert := 'select %s from %s %s ';

    LRttiField := LRttitype.GetFields;
    LFieldName := EmptyStr;
    for I := Low(LRttiField) to High(LRttiField) do
    begin
      LRttiAttribute := LRttiField[I].GetAttributes;
      for X := Low(LRttiAttribute) to High(LRttiAttribute) do
      begin
        if LRttiAttribute[X].ClassNameIs('TLocalField') then
        begin
          LMyField := LRttiAttribute[X] as TLocalField;

          if LMyField.FieldName.Equals('DataNascimento') then
            StrToInt('1');
          if Assigned(LMyField) and //
            (LMyField.FieldType <> TMyFieldType.mftObjectList) and //
            (LMyField.IsToSql) //
          then
          begin
            LReescreveAs := False;
            LMyFieldFieldName := LMyField.FieldName;

            if (LMyField.FieldType = mftDate) then
              StrToInt('1');

            if (LMyField.ValueToReturnNull.Trim.IsEmpty) then
              LFieldLocal := LMyFieldFieldName
            else
            begin
              LFieldLocal := 'coalesce(' + LMyFieldFieldName + ',' +
                LMyField.ValueToReturnNull + ')';
              LMyFieldFieldName := EmptyStr;
              LReescreveAs := True;
            end;

            case LLocalObject.FDriverDatabase of
              TDriverDatabase.ddbMySql8:
                begin
                  case LMyField.FieldType of
                    mftExtendedInt:
                      begin
                        LMyFieldFieldName := 'concat(' + LFieldLocal + ',"")';
                        LReescreveAs := True;
                      end;
                    mftDate:
                      begin
                        LMyFieldFieldName := 'DATE_FORMAT(' + LFieldLocal + ', '
                          + QuotedStr('%d/%m/%Y') + ')';
                        LReescreveAs := True;
                      end;
                  end;
                end;

              TDriverDatabase.ddbSqlite:
                begin
                  case LMyField.FieldType of
                    mftDate, mftDateTime:
                      begin
                        LMyFieldFieldName := 'strftime("' +
                          TCustomRttiSettings.FormatSqLiteDateTime + '", ' +
                          LFieldLocal + ')';
                        LReescreveAs := True;
                      end;
                  end;
                  //
                end;
            end;

            if not(LMyFieldFieldName.Trim.IsEmpty) then
              LFieldLocal := LMyFieldFieldName;

            if (LReescreveAs) then
              LFieldLocal := LFieldLocal + ' as ' + LMyField.FieldName;

            LFieldName := LFieldName + ',' + LFieldLocal;
          end;
        end;
      end;
    end;

    LSqlInsert := Format(LSqlInsert, [LFieldName.Trim([',']), LObjectName,
      ASqlWhere]);

    LSqlInsert := StringReplace(LSqlInsert, QuotedStr('null'), 'Null',
      [rfReplaceAll]);

    LSqlInsert := Trim(LSqlInsert);

    try
      LStrList := TStringList.Create;
      LStrList.Text := LSqlInsert;
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
      try
        LStrList.SaveToFile('Script' + Self.ClassName + '.GetSql.sql');
      except

      end;
{$ENDIF}
{$ENDIF}
      Result := LSqlInsert;
    finally
      LStrList.DisposeOf;
    end;
  end;
end;

procedure TCustomTable.LoadFromJson(const AJson: String);
var
  LObjectList: TObject;
  LJsonObj: System.Json.TJSONObject;
  LJsonArr: System.Json.TJSONArray;
  LJsonValue: System.Json.TJSONValue;
  LItem: System.Json.TJSONValue;
  LRttiField: TRttiField;
  LCustomField: TJsonField;
  LText: string;
begin
  LJsonArr := nil;
  LJsonValue := nil;
  LItem := nil;
  try
    LText := Copy(AJson, 0, 1);
    if (LText.Equals('[')) then
      LJsonArr := System.Json.TJSONObject.ParseJSONValue
        (TEncoding.UTF8.GetBytes(AJson), 0) as System.Json.TJSONArray
    else if (LText.Equals('{')) then
    begin
      LJsonArr := System.Json.TJSONArray.Create;
      LJsonObj := System.Json.TJSONObject.ParseJSONValue
        (TEncoding.UTF8.GetBytes(AJson), 0) as System.Json.TJSONObject;
      LJsonArr.AddElement(LJsonObj);
    end;

    for LJsonValue in LJsonArr do
    begin // Percorro tds os arrays

      for LItem in System.Json.TJSONArray(LJsonValue) do
      begin
        // Percorro todos os Index pegando os valores
        LRttiField := Self.GetField<TJsonField>(System.Json.TJSONPair(LItem).JsonString.Value, LCustomField, False);
        if (LRttiField <> nil) then
        begin
          { if (LCustomField.FieldType = mftObjectList) then
            begin
            LObjectList := LRttiField.GetValue(TObject(LObject)).AsObject;
            TCustomTableList<T>(LObjectList)
            .LoadFromJson(System.Json.TJSONPair(LItem).JsonValue.ToJson);
            end
            else }
          Self.SetFieldValue<TJsonField>(
            System.Json.TJSONPair(LItem).JsonString.Value,
            System.Json.TJSONPair(LItem).JsonValue.Value,
            False
            );
        end;
      end;
    end;
  finally
    LJsonArr.DisposeOf;
  end;
end;

function TCustomTable.PropertyExists(const AName: string): Boolean;
  function ExistProp(const PropName:string; List:TArray<TRttiProperty>) : Boolean;
  var
   Prop: TRttiProperty;
  begin
   result:=False;
    for Prop in List do
     if SameText(PropName, Prop.Name) then
     begin
       Result:=True;
       break;
     end;
  end;
var
  LCtx: TRttiContext;
  LObjType: TRttiType;
  LProp: TRttiProperty;
  CurrentClassProps : TArray<TRttiProperty>;
begin
  Result := False;

  LCtx := TRttiContext.Create;
  LObjType := LCtx.GetType(Self.ClassInfo);
  CurrentClassProps:= LObjType.GetDeclaredProperties;
   for LProp in LObjType.GetProperties do
     if AName.Equals(LProp.Name) then
     begin
       Result := True;
       Break;
     end;
   {if ExistProp(LProp.Name, CurrentClassProps) then
     Writeln(Format('The property %s is declarated in the current %s class',[Prop.Name, obj.ClassName]))
   else
     Writeln(Format('The property %s is declarated in the base class',[Prop.Name]))}
end;

procedure TCustomTable.SetDataSet(const Value: IDataSet);
begin
  FDataSet := Value;
end;

procedure TCustomTable.SetFieldValue(ALocalField: TLocalField;
  ARttiField: TRttiField; AFieldValue: TValue);
var
  LTryStrToInt: Integer;
  LTryStrToInt64: Int64;
  LTryStrToFloat: Double;
  LStr: string;
  LStr1: string;
  LDate: TDate;
  LDateTime: TDateTime;
  LDateFormat: string;
  LTimeFormat: string;
  LDateSeparator: Char;
begin
  if (Assigned(ARttiField)) then
  begin
    try
      if (ALocalField.FieldName = 'PRPROMOCAO') then
        StrToInt('1');
      if (ALocalField.ValueToReturnNull <> EmptyStr) and
        (AFieldValue.ToString.Trim.IsEmpty) then
      begin
        AFieldValue := ALocalField.FValueToReturnNull;
      end;

      case ALocalField.FieldType of
        mftObjectList:
          StrToInt('1');
        mftString, mftExtendedInt:
          AFieldValue := StringReplace(AFieldValue.ToString, #39, EmptyStr,
            [rfReplaceAll]);

        mftBooleanStr:
          begin
            AFieldValue := (UpperCase(AFieldValue.ToString) = 'TRUE');
          end;

        mftInteger:
          begin
            TryStrToInt(AFieldValue.ToString, LTryStrToInt);
            AFieldValue := LTryStrToInt;
          end;
        mftInt64:
          AFieldValue := StrToInt64(AFieldValue.ToString);
        mftDouble:
          begin
            LTryStrToFloat := 0;
            if not(TryStrToFloat(AFieldValue.ToString, LTryStrToFloat)) then
            begin
              TryStrToFloat(StringReplace(StringReplace(AFieldValue.ToString,
                ',', EmptyStr, [rfReplaceAll]), '.', ',', []), LTryStrToFloat);
            end;
            AFieldValue := LTryStrToFloat;
          end;

        mftDateTime, mftDate:
          begin
            LDateTime := 0;
            LStr := AFieldValue.ToString;
            if TryStrToInt(LStr, LTryStrToInt) then
              LDateTime := LTryStrToInt
            else if TryStrToInt64(LStr, LTryStrToInt64) then
              LDateTime := LTryStrToInt64
            else if TryStrToFloat(LStr, LTryStrToFloat) then
              LDateTime := LTryStrToFloat
            else
            begin
              LStr1 :=
                StringReplace(
                StringReplace(LStr,',', EmptyStr, [rfReplaceAll]), '.', ',', []
                );

              if TryStrToFloat(LStr1, LTryStrToFloat) then
                LDateTime := LTryStrToFloat
              else if (UpperCase(LStr).Equals('NULL')) or (LStr.Trim.IsEmpty) then
                LDateTime := 0
              else if ((Pos('-', LStr) > 0) or (Pos('/', LStr) > 0)) then
              begin
                LDateFormat := FormatSettings.ShortDateFormat;
                LTimeFormat := FormatSettings.ShortTimeFormat;
                LDateSeparator := FormatSettings.DateSeparator;
                try
                  if (Pos('-', LStr) > 0) then
                  begin
                    FormatSettings.DateSeparator := '-';
                    FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
                  end
                  else if (Pos('/', LStr) > 0) then
                  begin
                    FormatSettings.DateSeparator := '/';
                    FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
                  end;

                  FormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';

                  StrToDateTime(LStr);
                  LDateTime := StrToDateTime(LStr);
                finally
                  FormatSettings.ShortDateFormat := LDateFormat;
                  FormatSettings.ShortTimeFormat := LTimeFormat;
                  FormatSettings.DateSeparator := LDateSeparator;
                end;
              end
              else
                LDateTime := VarToDateTime(AFieldValue.AsVariant);;
            end;

            case ALocalField.FieldType of
              mftDate:
                begin
                  LDate := TDate(LDateTime);
                  AFieldValue := LDate;
                end;
              mftDateTime:
                begin
                  AFieldValue := LDateTime;
                end;
            end;
          end;
      end;
      ARttiField.SetValue(Self, AFieldValue);
    except
      on E: Exception do
        raise Exception.Create(ALocalField.FFieldName + ' - ' + E.Message);

    end;
  end;
end;

procedure TCustomTable.SetFieldValue<T>(const AFieldName: string;
  AFieldValue: TValue; const AShowException: Boolean);
var
  LLocalField: TLocalField;
  LRttiField: TRttiField;
  LText: string;
begin
  LRttiField := GetField<T>(AFieldName, LLocalField, AShowException);
  SetFieldValue(LLocalField, LRttiField, AFieldValue);
end;

function TCustomTable.ToJson(const AJsonFromSubItems: Boolean): string;
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LCount: Integer;
  LObjectList: TObject;
  LMyField: TJsonField;
  LSqlInsert: string;
  LFieldName: string;
  LFieldValueConcat: string;
  LText: string;
  LJson: string;
  LTryStrToFloat: Double;
  LDateTime: TDateTime;
  LStrList: TStringList;
begin
  try
    LSqlInsert := EmptyStr;
    LRttiContext := TRttiContext.Create;
    LRttitype := LRttiContext.GetType(Self.ClassType);

    LFieldValueConcat := EmptyStr;

    LRttitype := LRttiContext.GetType(Self.ClassType);
    LRttiField := LRttitype.GetFields;
    LFieldName := EmptyStr;
    for I := Low(LRttiField) to High(LRttiField) do
    begin
      LRttiAttribute := LRttiField[I].GetAttributes;
      for X := Low(LRttiAttribute) to High(LRttiAttribute) do
      begin
        if LRttiAttribute[X].ClassNameIs('TJsonField') then
        begin
          LMyField := LRttiAttribute[X] as TJsonField;
          if (Assigned(LMyField)) then
          begin
            if (LMyField.FieldType <> TMyFieldType.mftObjectList) then
            begin
              LFieldName := LFieldName + '"' + LMyField.FieldName + '":';
              // LText := VarToStr(Self.GetFieldValue(LMyField.FieldName));
              try
                case LMyField.FieldType of
                  mftString, mftBooleanStr, mftExtendedInt:
                  begin
                    LText := VarToStr(Self.GetFieldValue(LMyField.FieldName, False, ftJson));
                    {LText := StringReplace(LText, '/', '\/', [rfReplaceAll]);}
                    LText := StringReplace(LText, '"', EmptyStr, [rfReplaceAll]);
                    LFieldName := LFieldName + '"' + LText + '"';
                  end;
                  mftInteger, mftInt64:
                    LFieldName := LFieldName +
                      VarToStr(Self.GetFieldValue(LMyField.FieldName,
                      False, ftJson));
                  mftDouble:
                    LFieldName := LFieldName + '"' +
                      StringReplace
                      (VarToStr(Self.GetFieldValue(LMyField.FieldName, False,
                      ftJson)), ',', '.', [rfReplaceAll]) + '"';
                  mftDate:
                    begin
                      LText := VarToStr(Self.GetFieldValue(LMyField.FieldName,
                        False, ftJson));
                      LDateTime := 0;
                      if (Pos(':', LText) > 0) and (Pos('/', LText) = 0) then
                        TryStrToDate('30/12/2019 ' + LText, LDateTime)
                      else if (TryStrToFloat(LText, LTryStrToFloat)) then
                      begin
                        LDateTime := LTryStrToFloat;
                      end
                      else
                        TryStrToDate(LText, LDateTime);

                      LText := FloatToStr(LDateTime);
                      LFieldName := LFieldName + '"' +
                        FormatDateTime('yyyy-mm-dd', LDateTime) + '"';
                    end;
                  mftDateTime:
                    begin
                      LDateTime :=
                        VarToDateTime(Self.GetFieldValue(LMyField.FieldName,
                        False, ftJson));

                      LFieldName := LFieldName + '"' +
                        FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', LDateTime) + '"';
                    end;
                end;
                LFieldName := LFieldName + ',';
              except
                on E: Exception do
                begin
                  StrToInt('1');
                end;
              end;
            end
            else if (AJsonFromSubItems) then
            begin
              try
                LObjectList := Self.GetFieldObject(LMyField.FieldName, True,
                  TFieldType.ftJson);
                if (Assigned(LObjectList)) and (LObjectList <> nil) then
                begin
                  LCount := TCustomTableList<TCustomTable>(LObjectList).Count;
                  LJson := TCustomTableList<TCustomTable>(LObjectList)
                    .ToJson(AJsonFromSubItems);

                  if (LJson.Trim.IsEmpty) then
                    LJson := '[]';
                  // Verifico se a ultima posicao eh virgula, caso contrario, adiciono
                  LText := Copy(LFieldName, LFieldName.Length, 1);
                  if not(LText.Equals(',')) then
                    LFieldName := LFieldName + ',';
                  LFieldName := LFieldName + '"' + LMyField.FieldName +
                    '":' + LJson + ',';
                end;
              except
                on E: Exception do
                begin
                  raise Exception.Create(E.Message);
                end;
              end;
            end;

          end;
        end;
      end;
    end;

    LSqlInsert := '{' + LFieldName.Trim([',']) + '}';

    Result := LSqlInsert;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

{ TCustomTableList1<T> }

procedure TCustomTableList<T>.Add(AObject: T);
begin
  if (AObject = nil) then
  begin
    AObject := T.Create;
    FObjectList.Add(AObject)
  end
  else
    FObjectList.Add(AObject);
  if (Assigned(FDataSet)) then
    TCustomTable(FObjectList.Last).DataSet := FDataSet;
end;

procedure TCustomTableList<T>.ApplyUpdateFirebird3_0(const AObjectName: string;
  const AApplySubItems: Boolean);
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LObjectList: TObject;
  LMyField: TLocalField;
  LSqlInsert: string;
  LFieldName: string;
  LFieldValue: string;
  LLocalFieldValue: string;
  LFieldValueConcat: string;
  LMatching: string;

  LText: string;
  LDateTime: TDateTime;
  LStrList: TStringList;
  LObject: T;
begin
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  if not(AObjectName.IsEmpty) and (Count > 0) then
  begin
    for LObject in FObjectList do
    begin
      LSqlInsert := //
        'update or insert into ' + AObjectName + //
        ' (%s) values (%s) matching (%s)';
      LFieldValueConcat := EmptyStr;
      LMatching := EmptyStr;
      LRttitype := LRttiContext.GetType(LObject.ClassType);
      LRttiField := LRttitype.GetFields;
      LFieldName := EmptyStr;
      LFieldValue := EmptyStr;
      for I := Low(LRttiField) to High(LRttiField) do
      begin
        LRttiAttribute := LRttiField[I].GetAttributes;
        for X := Low(LRttiAttribute) to High(LRttiAttribute) do
        begin
          if LRttiAttribute[X].ClassNameIs('TLocalField') then
          begin
            LMyField := LRttiAttribute[X] as TLocalField;
            if (Assigned(LMyField)) then
            begin
              if (LMyField.DoPost) then
              begin
                if (LMyField.FieldType <> TMyFieldType.mftObjectList) then
                begin
                  begin
                    if (LMyField.DoMatch) then
                      LMatching := LMatching + ',' + LMyField.FieldName;

                    LFieldName := LFieldName + ',' + LMyField.FieldName;
                    LText := VarToStr
                      (LObject.GetFieldValue(LMyField.FieldName));
                    try
                      case LMyField.FieldType of
                        mftString, mftBooleanStr, mftExtendedInt:
                          LLocalFieldValue :=
                            QuotedStr(VarToStr
                            (LObject.GetFieldValue(LMyField.FieldName)));
                        mftInteger, mftInt64:
                          LLocalFieldValue :=
                            VarToStr(LObject.GetFieldValue(LMyField.FieldName));
                        mftDouble:
                          LLocalFieldValue :=
                            StringReplace
                            (VarToStr(LObject.GetFieldValue(LMyField.FieldName)
                            ), ',', '.', [rfReplaceAll]);
                        mftDate:
                          LLocalFieldValue := '''' +
                            FormatDateTime('yyyy-mm-dd',
                            StrToFloat
                            (VarToStr(LObject.GetFieldValue(LMyField.FieldName)
                            ))) + '''';
                        mftDateTime:
                          begin
                            LText := VarToStr
                              (LObject.GetFieldValue(LMyField.FieldName));

                            if (Pos(':', LText) > 0) and (Pos('/', LText) = 0)
                            then // Se for so Time
                              TryStrToDateTime('30/12/1899 ' + LText, LDateTime)
                            else
                              TryStrToDateTime(LText, LDateTime);
                            LText := DateTimeToStr(LDateTime);
                            LLocalFieldValue := '''' +
                              FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
                              LDateTime) + '''';
                          end;
                      end;

                      LFieldValue := LFieldValue + ',' + LLocalFieldValue;
                    except
                      on E: Exception do
                      begin
                        StrToInt('1');
                      end;
                    end;
                  end;
                end
                else if (AApplySubItems) then
                begin
                  LObjectList := LObject.GetFieldObject(LMyField.FieldName);
                  if (Assigned(LObjectList)) and (LObjectList <> nil) then
                  begin
                    TCustomTableList<T>(LObjectList)
                      .ApplyUpdates(AApplySubItems);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      { if (LMatching.Trim.IsEmpty) then
        raise Exception.Create
        ('Rtti ApplyUpdateFirebird3_0, Matching is required'); }

      LFieldValueConcat := LFieldValueConcat + LFieldValue.Trim([',']);

      LSqlInsert := Format(LSqlInsert, [LFieldName.Trim([',']),
        LFieldValueConcat.Trim([',']), LMatching.Trim([','])]);

      if (LMatching.Trim.IsEmpty) then
        LSqlInsert := StringReplace(LSqlInsert, ' matching ()', EmptyStr,
          [rfReplaceAll]);

      try
        LStrList := TStringList.Create;
        LStrList.Text := StringReplace(LSqlInsert, QuotedStr('null'), 'Null',
          [rfReplaceAll]);
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
        try
          LStrList.SaveToFile('Script' + Self.ClassName +
            '.ApplyUpdateFirebird3_0.sql');
        except

        end;
{$ENDIF}
{$ENDIF}
        DataSet.ConnExec(LStrList.Text);
      finally
        LStrList.DisposeOf;
      end;
    end;
  end;
end;

procedure TCustomTableList<T>.ApplyUpdateMySql8(const AObjectName: string;
  const AApplySubItems: Boolean);
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LObjectList: TObject;
  LMyField: TLocalField;
  LSqlInsert: string;
  LSqlInsertExec: string;
  LFieldName: string;
  LFieldValue: string;
  LLocalFieldValue: string;
  LFieldValueConcat: string;
  LMatching: string;

  LText: string;
  LDateTime: TDateTime;
  LStrList: TStringList;
  LObject: T;
  LFirstObject: Boolean;
begin
  LFirstObject := True;
  LMatching := EmptyStr;
  LFieldName := EmptyStr;
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  if not(AObjectName.IsEmpty) and (Count > 0) then
  begin
    LSqlInsert := 'insert into ' + AObjectName +
      ' (%s) values %s on duplicate key update %s';
    LFieldValueConcat := '';
    for LObject in FObjectList do
    begin
      LRttitype := LRttiContext.GetType(LObject.ClassType);
      LRttiField := LRttitype.GetFields;
      LFieldValue := EmptyStr;
      for I := Low(LRttiField) to High(LRttiField) do
      begin
        LRttiAttribute := LRttiField[I].GetAttributes;
        for X := Low(LRttiAttribute) to High(LRttiAttribute) do
        begin
          if LRttiAttribute[X].ClassNameIs('TLocalField') then
          begin
            LMyField := LRttiAttribute[X] as TLocalField;
            if (Assigned(LMyField)) then
            begin
              LText := VarToStr(LObject.GetFieldValue(LMyField.FieldName));
              if (LMyField.DoPost) and //
                not((LMyField.IsAutoInc) and (LText.Equals('-1'))) //
              then
              begin
                if (LMyField.FieldType <> TMyFieldType.mftObjectList) then
                begin
                  begin
                    if (LMyField.FieldName = 'Observacao') then
                      StrToInt('1');
                    if (LFirstObject) then
                    begin
                      if not(LMyField.DoMatch) then
                        LMatching := LMatching + ',' + LMyField.FieldName +
                          '=VALUES(' + LMyField.FieldName + ')';

                      LFieldName := LFieldName + ',' + LMyField.FieldName;
                    end;

                    try
                      case LMyField.FieldType of
                        mftString, mftBooleanStr, mftExtendedInt:
                          LLocalFieldValue :=
                            QuotedStr(VarToStr
                            (LObject.GetFieldValue(LMyField.FieldName)));
                        mftInteger, mftInt64:
                          LLocalFieldValue :=
                            VarToStr(LObject.GetFieldValue(LMyField.FieldName));
                        mftDouble:
                          LLocalFieldValue :=
                            StringReplace
                            (VarToStr(LObject.GetFieldValue(LMyField.FieldName)
                            ), ',', '.', [rfReplaceAll]);
                        mftDate:
                          LLocalFieldValue := '''' +
                            FormatDateTime('yyyy-mm-dd',
                            StrToFloat
                            (VarToStr(LObject.GetFieldValue(LMyField.FieldName)
                            ))) + '''';
                        mftDateTime:
                          begin
                            LText := VarToStr
                              (LObject.GetFieldValue(LMyField.FieldName));

                            if (Pos(':', LText) > 0) and (Pos('/', LText) = 0)
                            then
                              TryStrToDateTime('30/12/2019 ' + LText, LDateTime)
                            else
                              TryStrToDateTime(LText, LDateTime);

                            LLocalFieldValue := '''' +
                              FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
                              LDateTime) + '''';
                          end;
                      end;

                      if not(LMyField.ValueToReturnNull.Trim.IsEmpty) and
                        (LMyField.ValueToReturnNull = LLocalFieldValue) then
                        LLocalFieldValue := 'Null';

                      LFieldValue := LFieldValue + ',' + LLocalFieldValue;
                    except
                      on E: Exception do
                      begin
                        StrToInt('1');
                      end;
                    end;
                  end;
                end
                else if (AApplySubItems) then
                begin
                  LObjectList := LObject.GetFieldObject(LMyField.FieldName);
                  if (Assigned(LObjectList)) and (LObjectList <> nil) then
                  begin
                    TCustomTableList<T>(LObjectList)
                      .ApplyUpdates(AApplySubItems);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      LFieldValueConcat := LFieldValueConcat + '( ' +
        LFieldValue.Trim([',']) + '),';

      LFirstObject := False;
    end;

    LSqlInsert := //
      Format( //
      LSqlInsert, //
      [ //
      LFieldName.Trim([',']), //
      LFieldValueConcat.Trim([',']), //
      LMatching.Trim([',']) //
      ] //
      );

    { LSqlInsert := //
      Format( //
      LSqlInsert, //
      [ //
      LFieldName.Trim([',']), //
      LFieldValueConcat.Trim([',']), //
      LMatching.Trim([',']) //
      ] //
      ); }

    if (LMatching.Trim.IsEmpty) then
      LSqlInsert := StringReplace(LSqlInsert, ' matching ()', '',
        [rfReplaceAll]);

    try
      LStrList := TStringList.Create;
      LStrList.Text := StringReplace(LSqlInsert, QuotedStr('null'), 'Null',
        [rfReplaceAll]);
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
      try
        LStrList.SaveToFile('Script' + Self.ClassName +
          '.ApplyUpdateMySql8.sql');
      except

      end;
{$ENDIF}
{$ENDIF}
      DataSet.ConnExec(LStrList.Text);
    finally
      LStrList.DisposeOf;
    end;
  end;
end;

procedure TCustomTableList<T>.ApplyUpdates(const AApplySubItems: Boolean);
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiAttribute: TArray<TCustomAttribute>;
  X: Integer;
  LLocalObject: TLocalObject;
  LObjectName: string;
  LDriverDatabase: TDriverDatabase;
begin
  LObjectName := EmptyStr;
  LDriverDatabase := ddbNone;
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  LRttiAttribute := LRttitype.GetAttributes;
  for X := Low(LRttiAttribute) to High(LRttiAttribute) do
  begin
    if LRttiAttribute[X].ClassNameIs('TLocalObject') then
    begin
      LLocalObject := LRttiAttribute[X] as TLocalObject;
      if Assigned(LLocalObject) then
      begin
        LObjectName := LLocalObject.ObjectName;
        LDriverDatabase := LLocalObject.FDriverDatabase;
        Break;
      end;
    end;
  end;

  if (LObjectName.Trim.IsEmpty) then
    raise Exception.Create('Rtti Objectname is required to ApplyUpdates');

  case LDriverDatabase of
    ddbNone:
      raise Exception.Create('Rtti DriverDatabase is required to ApplyUpdates');
    ddbSqlite:
      ApplyUpdateSqlite(LObjectName, AApplySubItems);
    ddbFirebird3_0:
      ApplyUpdateFirebird3_0(LObjectName, AApplySubItems);
    ddbMySql8:
      ApplyUpdateMySql8(LObjectName, AApplySubItems);
  end;
end;

procedure TCustomTableList<T>.ApplyUpdateSqlite(const AObjectName: string;
  const AApplySubItems: Boolean);
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LObjectList: TObject;
  LMyField: TLocalField;
  LSqlInsert: string;
  LFieldName: string;
  LFieldValue: string;
  LLocalFieldValue: string;
  LFieldValueConcat: string;

  LText: string;
  LDateTime: TDateTime;
  LStrList: TStringList;
  LObject: T;
begin
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  if not(AObjectName.IsEmpty) and (Count > 0) then
  begin
    LSqlInsert := 'insert or replace into ' + AObjectName + ' (%s) values %s';
    LFieldValueConcat := EmptyStr;
    for LObject in FObjectList do
    begin
      LRttitype := LRttiContext.GetType(LObject.ClassType);
      LRttiField := LRttitype.GetFields;
      LFieldName := EmptyStr;
      LFieldValue := EmptyStr;
      for I := Low(LRttiField) to High(LRttiField) do
      begin
        LRttiAttribute := LRttiField[I].GetAttributes;
        for X := Low(LRttiAttribute) to High(LRttiAttribute) do
        begin
          if LRttiAttribute[X].ClassNameIs('TLocalField') then
          begin
            LMyField := LRttiAttribute[X] as TLocalField;
            if (Assigned(LMyField)) then
            begin
              if (LMyField.DoPost) and not(LMyField.IsAutoInc) then
              begin
                if (LMyField.FieldType <> TMyFieldType.mftObjectList) then
                begin
                  // if (VarToStr(LObject.GetFieldValue(LMyField.FieldName)) <> '-1')then
                  begin
                    LFieldName := LFieldName + ',' + LMyField.FieldName;

                    if (LMyField.FieldName = 'ExpirationDate') then
                      StrToInt('1');

                    LText := VarToStr
                      (LObject.GetFieldValue(LMyField.FieldName));
                    try
                      case LMyField.FieldType of
                        mftString, mftBooleanStr, mftExtendedInt:
                          LLocalFieldValue :=
                            QuotedStr(VarToStr
                            (LObject.GetFieldValue(LMyField.FieldName)));
                        mftInteger, mftInt64:
                          LLocalFieldValue :=
                            VarToStr(LObject.GetFieldValue(LMyField.FieldName));
                        mftDouble:
                          LLocalFieldValue :=
                            StringReplace
                            (VarToStr(LObject.GetFieldValue(LMyField.FieldName)
                            ), ',', '.', [rfReplaceAll]);
                        mftDate:
                          LLocalFieldValue := '"' + FormatDateTime('yyyy-mm-dd',
                            StrToFloat
                            (VarToStr(LObject.GetFieldValue
                            (LMyField.FieldName)))) + '"';
                        mftDateTime:
                          begin
                            LText := VarToStr
                              (LObject.GetFieldValue(LMyField.FieldName));

                            if (Pos(':', LText) > 0) and (Pos('/', LText) = 0)
                            then
                              TryStrToDateTime('30/12/2019 ' + LText, LDateTime)
                            else
                              TryStrToDateTime(LText, LDateTime);

                            LLocalFieldValue :=
                              '"' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
                              LDateTime) + '"';
                          end;
                      end;

                      LFieldValue := LFieldValue + ',' + LLocalFieldValue;
                    except
                      on E: Exception do
                      begin
                        StrToInt('1');
                      end;
                    end;
                  end;
                end
                else if (AApplySubItems) then
                begin
                  LObjectList := LObject.GetFieldObject(LMyField.FieldName);
                  if (Assigned(LObjectList)) and (LObjectList <> nil) then
                  begin
                    TCustomTableList<T>(LObjectList)
                      .ApplyUpdates(AApplySubItems);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      LFieldValueConcat := LFieldValueConcat + '(' +
        LFieldValue.Trim([',']) + '),';
    end;
    LSqlInsert := Format(LSqlInsert, [LFieldName.Trim([',']),
      LFieldValueConcat.Trim([','])]);

    try
      LStrList := TStringList.Create;
      LStrList.Text := StringReplace(LSqlInsert, QuotedStr('null'), 'Null',
        [rfReplaceAll]);
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
      try
        LStrList.SaveToFile('Script' + Self.ClassName +
          '.ApplyUpdateSqlite.sql');
      except

      end;
{$ENDIF}
{$ENDIF}
      DataSet.ConnExec(LStrList.Text);
    finally
      LStrList.DisposeOf;
    end;
  end;
end;

procedure TCustomTableList<T>.Clear;
begin
  ObjectList.Clear;
end;

function TCustomTableList<T>.Count: Integer;
begin
  Result := FObjectList.Count;
end;

constructor TCustomTableList<T>.Create(ADataSet: IDataSet);
begin
  Create();
  FDataSet := ADataSet;
end;

constructor TCustomTableList<T>.Create;
begin
  FDataSet := nil;
  FObjectList := TObjectList<T>.Create;
end;

destructor TCustomTableList<T>.Destroy;
begin
  FObjectList.DisposeOf;
  inherited;
end;

function TCustomTableList<T>.First: T;
begin
  Result := nil;
  if FObjectList.Count > 0 then
    Result := FObjectList.First;
end;

function TCustomTableList<T>.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TCustomTableList<T>.Item(const AIndex: Integer): T;
begin
  Result := FObjectList.Items[AIndex];
end;

function TCustomTableList<T>.Last: T;
begin
  Result := FObjectList.Last;
end;

procedure TCustomTableList<T>.LoadFromJson(const AJson: String);
var
  LJsonObj: System.Json.TJSONObject;
  LJsonArr: System.Json.TJSONArray;
  LJsonValue: System.Json.TJSONValue;
  LItem: System.Json.TJSONValue;
  LObject: T;
  LObjectList: TObject;
  LRttiField: TRttiField;
  LCustomField: TJsonField;
  LCount: Integer;
  LText: string;
  LJson: string;
begin
  try
    LJsonArr := nil;
    LJsonValue := nil;
    LItem := nil;
    LCount := 0;
    LJson := AJson;
    if (LJson.Trim.IsEmpty) then
      LJson := '[]';
    // Recebo o JsonArray
    LText := Copy(LJson, 0, 1);
    if (LText.Equals('{')) then
      LJson := '[' + LJson + ']';

    LJsonArr := System.Json.TJSONObject.ParseJSONValue
      (TEncoding.UTF8.GetBytes(LJson), 0) as System.Json.TJSONArray;

    { if (LText.Equals('[')) then
      LJsonArr := System.Json.TJSONObject.ParseJSONValue
      (TEncoding.UTF8.GetBytes(LJson), 0) as System.Json.TJSONArray
      else if (LText.Equals('{')) then
      begin
      LJsonArr := System.Json.TJSONArray.Create;
      LJsonObj := System.Json.TJSONObject.ParseJSONValue
      (TEncoding.UTF8.GetBytes(AJson), 0) as System.Json.TJSONObject;
      LJsonArr.AddElement(LJsonObj);
      end; }

    for LJsonValue in LJsonArr do
    begin // Percorro tds os arrays
      LObject := T.Create;
      LObject.DataSet := FDataSet;

      for LItem in System.Json.TJSONArray(LJsonValue) do
      begin
        // Percorro todos os Index pegando os valores
        LObjectList := nil;

        LRttiField := LObject.GetField<TJsonField>(System.Json.TJSONPair(LItem)
          .JsonString.Value, LCustomField, False);
        if (LRttiField <> nil) then
        begin
          if (LCustomField.FieldType = mftObjectList) then
          begin
            LObjectList := LRttiField.GetValue(TObject(LObject)).AsObject;
            TCustomTableList<T>(LObjectList).DataSet := FDataSet;
            TCustomTableList<T>(LObjectList)
              .LoadFromJson(System.Json.TJSONPair(LItem).JsonValue.ToJson);
          end
          else
          begin
            LObject.SetFieldValue<TJsonField>(
            System.Json.TJSONPair(LItem).JsonString.Value,
            System.Json.TJSONPair(LItem).JsonValue.Value,
            False);
          end;
        end;
      end;
      Add(LObject);
    end;
  finally
    LJsonArr.DisposeOf;
  end;
end;

procedure TCustomTableList<T>.LoadFromSql(const ASql: string);
var
  LObject: T;
  I: Integer;
  LRttiField: TRttiField;
  LLocalField: TLocalField;
  LValue: TValue;
  LDsListagem: TDataSet;
  LText: string;
  LCount: Integer;
begin
  try
    LDsListagem := nil;

    LDsListagem := DataSet.GetDataSet(ASql);

    if (LDsListagem.FindField('DataHora') <> nil) then
    begin
      LCount := LDsListagem.RecordCount;
      LDsListagem.First;
      while not(LDsListagem.Eof) do
      begin
        LText := LDsListagem.FieldByName('DataHora').AsString;
        LDsListagem.Next;
      end;
    end;

    LDsListagem.First;
    while not(LDsListagem.Eof) do
    begin
      LObject := T.Create(DataSet);
      Add(LObject);

      for I := 0 to Pred(LDsListagem.Fields.Count) do
      begin
        try
          LText := LDsListagem.Fields[I].FieldName;
          if (LDsListagem.Fields[I].FieldName = 'DataInsert') then
            StrToInt('1');

          LRttiField := LObject.GetField<TLocalField>
            (LDsListagem.Fields[I].FieldName, LLocalField, False);

          if (LRttiField <> nil) then
          begin
            LObject.SetFieldValue(LLocalField, LRttiField,
              LDsListagem.Fields[I].AsString);
          end;
        except
        StrToInt('1');
        end;
      end;

      LDsListagem.Next;
    end;

    LDsListagem.Close;
  finally
    FreeAndNil(LDsListagem);
  end;
end;

procedure TCustomTableList<T>.Delete(const AIndex: Integer);
begin
  FObjectList.Delete(AIndex);
end;

procedure TCustomTableList<T>.SaveJsonToFile(const AFile: string;
  const AJsonFromSubItems: Boolean);
var
  LJson: string;
  LStrListr: TStringList;
begin
  LJson := ToJson(AJsonFromSubItems);

  try
    LStrListr := TStringList.Create;
    LStrListr.Add(LJson);
    while True do
    begin
      try
        LStrListr.SaveToFile(AFile);
        Break;
      except

      end;
    end;
  finally
    FreeAndNil(LStrListr);
  end;
end;

procedure TCustomTableList<T>.SetDataSet(const Value: IDataSet);
begin
  FDataSet := Value;
end;

procedure TCustomTableList<T>.SetObjectList(const Value: TObjectList<T>);
begin
  FObjectList := Value;
end;

procedure TCustomTableList<T>.SortBy(const AFieldName, ATypeOrder: string;
  const AFieldType: TFieldType);
var
  LMultiply: Integer;
begin
  LMultiply := IfThen(ATypeOrder.Equals('desc'), -1, 1);

  Self.ObjectList.Sort(TComparer<T>.Construct(
    function(const LLeft, LRight: T): Integer
    begin
      if //
        LLeft.GetFieldValue(AFieldName, False, AFieldType) = //
        LRight.GetFieldValue(AFieldName, False, AFieldType) //
      then
        Result := 0
      else if //
        LLeft.GetFieldValue(AFieldName, False, AFieldType) < //
        LRight.GetFieldValue(AFieldName, False, AFieldType) //
      then
        Result := -1 * LMultiply
      else
        Result := 1 * LMultiply;
    end));
end;

procedure TCustomTableList<T>.SortByJson(const AFieldName, ATypeOrder: string);
begin
  SortBy(AFieldName, ATypeOrder, TFieldType.ftJson);
end;

function TCustomTableList<T>.ToJson(const AJsonFromSubItems: Boolean): string;
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LCount: Integer;
  LObjectList: TObject;
  LMyField: TJsonField;
  LSqlInsert: string;
  LFieldName: string;
  LFieldValueConcat: string;
  LText: string;

  LDateTime: TDateTime;
  LStrList: TStringList;
  LObject: T;
begin
  LSqlInsert := EmptyStr;
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);
  LCount := Count;
  if (Count > 0) then
  begin
    LFieldValueConcat := EmptyStr;
    for LObject in Self.ObjectList do
    begin
      if (LObject = nil) then
        StrToInt('1');
      LFieldName := LObject.ToJson(AJsonFromSubItems);
      LFieldValueConcat := LFieldValueConcat + LFieldName.Trim([',']) + ',';
    end;
    LSqlInsert := '[' + LFieldValueConcat.Trim([',']) + ']';
  end;

  if (LSqlInsert.Trim.IsEmpty) then
    LSqlInsert := '[]';

  Result := LSqlInsert;
end;

function TCustomTableList<T>.LocateValue(const AFieldName: string;
  AValue: Variant): T;
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LMyField: TCustomField;
  LFieldType: TFieldType;
  LFieldName: string;
  LObject: T;
  LValue: Variant;
begin
  Result := nil;
  LValue := null;
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  if (Count > 0) then
  begin
    for LObject in FObjectList do
    begin
      LRttitype := LRttiContext.GetType(LObject.ClassType);
      LRttiField := LRttitype.GetFields;

      for I := Low(LRttiField) to High(LRttiField) do
      begin
        LRttiAttribute := LRttiField[I].GetAttributes;
        for X := Low(LRttiAttribute) to High(LRttiAttribute) do
        begin
          LMyField := nil;
          LFieldName := EmptyStr;
          LValue := null;

          LMyField := LRttiAttribute[X] as TCustomField;
          LFieldName := LMyField.FieldName;
          if (Assigned(LMyField)) then
          begin
            LFieldName := LMyField.FieldName;
             //
            if (LFieldName.Equals(AFieldName)) and //
            (LMyField.FieldType <> TMyFieldType.mftObjectList) //
            then
            begin
              LFieldType := TFieldType.ftLocal;
              if LRttiAttribute[X].ClassNameIs('TJsonField') then
                LFieldType := TFieldType.ftJson;
              LValue := LObject.GetFieldValue(LMyField.FieldName, True, LFieldType);
            end;
          end;
          if not(LFieldName.Trim.IsEmpty) and (LValue <> null) and (AValue = LValue) then
          begin
            Result := LObject;
            Break;
          end;
        end;
        if (Assigned(Result)) then
          Break;
      end;

      if (Assigned(Result)) then
        Break;
    end;
  end;
end;

function TCustomTableList<T>.ToJsonFromSql(const ASqlWhere: string): string;
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiAttribute: TArray<TCustomAttribute>;
  X: Integer;
  LLocalObject: TLocalObject;
  LObjectName: string;
  LDriverDatabase: TDriverDatabase;
begin
  LObjectName := EmptyStr;
  LDriverDatabase := ddbNone;
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  LRttiAttribute := LRttitype.GetAttributes;
  for X := Low(LRttiAttribute) to High(LRttiAttribute) do
  begin
    if LRttiAttribute[X].ClassNameIs('TLocalObject') then
    begin
      LLocalObject := LRttiAttribute[X] as TLocalObject;
      if Assigned(LLocalObject) then
      begin
        LObjectName := LLocalObject.ObjectName;
        LDriverDatabase := LLocalObject.FDriverDatabase;
        Break;
      end;
    end;
  end;

  if (LObjectName.Trim.IsEmpty) then
    raise Exception.Create('Rtti Objectname is required to ToJsonFromSql');

  case LDriverDatabase of
    ddbNone:
      raise Exception.Create
        ('Rtti DriverDatabase is required to ToJsonFromSql');
    ddbSqlite:
      Result := ToJsonFromSqlSqlite(LObjectName, ASqlWhere);
    ddbFirebird3_0:
      Result := ToJsonFromSqlFirebird3_0(LObjectName, ASqlWhere);
  end;
end;

function TCustomTableList<T>.ToJsonFromSqlFirebird3_0(const AObjectName,
  ASqlWhere: string): string;
const
  cStringDate = 'LPAD(EXTRACT(YEAR FROM AFieldName' +
    '), 4, ''0'') || ''-'' || LPAD(EXTRACT(MONTH FROM AFieldName' +
    '), 2, ''0'') || ''-'' || LPAD(EXTRACT(DAY FROM AFieldName' +
    '), 2, ''0'')';
  cStringDateTime = 'LPAD(EXTRACT(YEAR FROM AFieldName ' +
    '), 4, ''0'') || ''-'' || LPAD(EXTRACT(MONTH FROM AFieldName ' +
    '), 2, ''0'') || ''-'' || LPAD(EXTRACT(DAY FROM AFieldName ' +
    '), 2, ''0'') ||'' '' || LPAD(EXTRACT(HOUR FROM AFieldName ' +
    '), 2, ''0'') || '':'' || LPAD(EXTRACT(MINUTE FROM AFieldName ' +
    '), 2, ''0'') || '':'' || LPAD(EXTRACT(SECOND FROM AFieldName ' +
    '), 2, ''0'') || ''.'' || LPAD(EXTRACT(MILLISECOND FROM AFieldName ' +
    '), 2, ''0'')';
var
  LRttiContext: TRttiContext;
  LRttitype: TRttiType;
  LRttiField: TArray<TRttiField>;
  LRttiAttribute: TArray<TCustomAttribute>;
  X, I: Integer;
  LObjectList: TObject;
  LMyField: TLocalField;
  LSqlInsert: string;
  LFieldName: string;
  LLocalFieldValue: string;
  LFieldValueConcat: string;

  LText: string;
  LDateTime: TDateTime;
  LStrList: TStringList;
  LObject: T;
  LDataSet: TDataSet;

begin
  Result := EmptyStr;
  LRttiContext := TRttiContext.Create;
  LRttitype := LRttiContext.GetType(Self.ClassType);

  if not(AObjectName.IsEmpty) then
  begin
    for LObject in FObjectList do
    begin
      LRttitype := LRttiContext.GetType(LObject.ClassType);
      LRttiField := LRttitype.GetFields;
      LFieldName := EmptyStr;
      for I := Low(LRttiField) to High(LRttiField) do
      begin
        LRttiAttribute := LRttiField[I].GetAttributes;
        for X := Low(LRttiAttribute) to High(LRttiAttribute) do
        begin
          LText := LRttiAttribute[X].ClassName;
          if LRttiAttribute[X].ClassNameIs('TLocalField') then
          begin
            LMyField := LRttiAttribute[X] as TLocalField;
            if (Assigned(LMyField)) then
            begin
              if (LMyField.DoPost) then
              begin
                if (LMyField.FieldType <> TMyFieldType.mftObjectList) then
                begin
                  LFieldName := LFieldName + '*_1_*"' + LMyField.FieldName +
                    '":*_1_*';
                  try
                    case LMyField.FieldType of
                      mftString, mftBooleanStr, mftDouble, mftInteger, mftInt64,
                        mftExtendedInt:
                        LFieldName := LFieldName + '||' + '*_1_*"*_1_*' + '||' +
                          LMyField.FieldName + '||' + '*_1_*"*_1_*';

                      mftDate:
                        LFieldName := LFieldName + '||' + '*_1_*"*_1_*' + '||' +
                          StringReplace(cStringDate, 'AFieldName',
                          LMyField.FieldName, [rfReplaceAll]) + '||' +
                          '*_1_*"*_1_*';

                      mftDateTime:
                        LFieldName := LFieldName + '||' + '*_1_*"*_1_*' + '||' +
                          StringReplace(cStringDateTime, 'AFieldName',
                          LMyField.FieldName, [rfReplaceAll]) + '||' +
                          '*_1_*"*_1_*';
                    end;

                    LFieldName := LFieldName + '||' + QuotedStr(',') + '||';
                  except
                    on E: Exception do
                    begin
                      StrToInt('1');
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      // So precisa rodar 1 vez
      Break;
    end;

    LFieldValueConcat := StringReplace(LFieldName, '*_1_*', #39,
      [rfReplaceAll]);

    LFieldValueConcat := Copy(LFieldValueConcat, 0,
      LFieldValueConcat.Length - 7);

    LSqlInsert :=
      'SELECT LIST(''{''||JsonRow||''}'', '','') AS Json FROM(select ' +
      LFieldValueConcat + ' as JsonRow from ' + AObjectName + ' ' +
      ASqlWhere + ')';

    try
      LStrList := TStringList.Create;
      LStrList.Text := LSqlInsert;
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
      try
        LStrList.SaveToFile('Script' + Self.ClassName +
          '.ToJsonFromSqlFirebird3_0.sql');
      except

      end;
{$ENDIF}
{$ENDIF}
      LDataSet := DataSet.GetDataSet(LSqlInsert);
      Result := LDataSet.FieldByName('Json').AsString;
    finally
      FreeAndNil(LDataSet);
      LStrList.DisposeOf;
    end;
  end;
end;

function TCustomTableList<T>.ToJsonFromSqlMySql8(const AObjectName,
  ASqlWhere: string): string;
begin

end;

function TCustomTableList<T>.ToJsonFromSqlSqlite(const AObjectName,
  ASqlWhere: string): string;
begin

end;

{ TCustomRttiSettings }

class procedure TCustomRttiSettings.SetFormatSqLiteDateTime
  (const Value: string);
begin
  FFormatSqLiteDateTime := Value;
end;

end.
