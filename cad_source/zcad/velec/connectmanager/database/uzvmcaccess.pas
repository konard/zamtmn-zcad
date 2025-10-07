{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}
{$mode objfpc}{$H+}

unit uzvmcaccess;
{$INCLUDE zengineconfig.inc}

interface
uses
   sysutils, Classes, Dialogs, gvector,
   SQLDB, odbcconn,
   uzcinterface, uzcvariablesutils, varmandef,
   uzvmcdrawing;

type
  TDeviceInfoForExport = record
    nameDev: string;
    phase: string;
    typeKc: string;
    power: double;
    volt: double;
    cosPhi: double;
  end;

  TAccessDBExporter = class
  private
    FConnection: TODBCConnection;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    FDatabasePath: string;
  public
    constructor Create(const ADatabasePath: string);
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure ClearTables;
    procedure ExportDevice(const ADeviceInfo: TDeviceInfoForExport);
    procedure ExportConnection(const APrimID, ASecID, AFeeder: string);
    procedure ExportDevicesFromDrawing(ADeviceCollector: TDeviceDataCollector);
    procedure ExportConnectionsFromDrawing(ADeviceCollector: TDeviceDataCollector);
    procedure Commit;

    property DatabasePath: string read FDatabasePath write FDatabasePath;
  end;

implementation

constructor TAccessDBExporter.Create(const ADatabasePath: string);
begin
  inherited Create;
  FDatabasePath := ADatabasePath;

  FConnection := TODBCConnection.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);

  FConnection.Driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
  FConnection.LoginPrompt := False;
end;

destructor TAccessDBExporter.Destroy;
begin
  Disconnect;
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TAccessDBExporter.Connect;
begin
  if FConnection.Connected then
    Exit;

  FConnection.Params.Clear;
  FConnection.Params.Add('Dbq=' + FDatabasePath);
  FConnection.Connected := True;

  FTransaction.DataBase := FConnection;
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;

  zcUI.TextMessage('Connected to Access database: ' + FDatabasePath, TMWOHistoryOut);
end;

procedure TAccessDBExporter.Disconnect;
begin
  if FConnection.Connected then
    FConnection.Connected := False;
end;

procedure TAccessDBExporter.ClearTables;
begin
  try
    FQuery.SQL.Text := 'DELETE * FROM Device';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'DELETE * FROM Connect';
    FQuery.ExecSQL;

    zcUI.TextMessage('Access tables cleared', TMWOHistoryOut);
  except
    on E: Exception do
      zcUI.TextMessage('Ошибка очищения таблиц: ' + E.Message, TMWOHistoryOut);
  end;
end;

procedure TAccessDBExporter.ExportDevice(const ADeviceInfo: TDeviceInfoForExport);
begin
  try
    FQuery.SQL.Text := 'INSERT INTO Device (Prim_ID, Рower, Voltage, Phase, CosF) VALUES (:pPrimID, :pPower, :pVoltage, :pPhase, :pCosF)';
    FQuery.Params.ParamByName('pPrimID').AsString := ADeviceInfo.nameDev;
    FQuery.Params.ParamByName('pPower').AsFloat := ADeviceInfo.power;
    FQuery.Params.ParamByName('pVoltage').AsFloat := ADeviceInfo.volt;
    FQuery.Params.ParamByName('pPhase').AsString := ADeviceInfo.phase;
    FQuery.Params.ParamByName('pCosF').AsFloat := ADeviceInfo.cosPhi;
    FQuery.ExecSQL;
  except
    on E: Exception do
      zcUI.TextMessage('Ошибка экспорта устройства: ' + E.Message, TMWOHistoryOut);
  end;
end;

procedure TAccessDBExporter.ExportConnection(const APrimID, ASecID, AFeeder: string);
begin
  try
    FQuery.SQL.Text := 'INSERT INTO Connect (Prim_ID, Sec_ID, Feeder) VALUES (:pPrimID, :pSecID, :pFeeder)';
    FQuery.Params.ParamByName('pPrimID').AsString := APrimID;
    FQuery.Params.ParamByName('pSecID').AsString := ASecID;
    FQuery.Params.ParamByName('pFeeder').AsString := AFeeder;
    FQuery.ExecSQL;
  except
    on E: Exception do
      zcUI.TextMessage('Ошибка экспорта подключения: ' + E.Message, TMWOHistoryOut);
  end;
end;

procedure TAccessDBExporter.Commit;
begin
  FTransaction.Commit;
  zcUI.TextMessage('Access export committed successfully', TMWOHistoryOut);
end;

procedure TAccessDBExporter.ExportDevicesFromDrawing(ADeviceCollector: TDeviceDataCollector);
var
  devices: specialize TVector<TDeviceData>;
  i: integer;
  deviceInfo: TDeviceInfoForExport;
  pdev: PGDBObjDevice;
  pvd: pvardesk;
  strTemp: string;
begin
  devices := ADeviceCollector.CollectAllDevices;

  try
    for i := 0 to devices.Size - 1 do
    begin
      // Get the device object to extract additional properties
      pdev := ADeviceCollector.GetDeviceByName(devices[i].DevName);
      if pdev = nil then
        Continue;

      deviceInfo.nameDev := devices[i].DevName;

      // Extract voltage
      pvd := FindVariableInEnt(pdev, 'Voltage');
      if pvd <> nil then
      begin
        strTemp := pvd^.data.ptd^.GetValueAsString(pvd^.data.Addr.Instance);
        if strTemp = '_AC_380V_50Hz' then
          deviceInfo.volt := 380
        else if strTemp = '_AC_220V_50Hz' then
          deviceInfo.volt := 220
        else
          deviceInfo.volt := -110;
      end
      else
        deviceInfo.volt := -110;

      // Extract phase
      pvd := FindVariableInEnt(pdev, 'Phase');
      if pvd <> nil then
      begin
        strTemp := pvd^.data.ptd^.GetValueAsString(pvd^.data.Addr.Instance);
        if strTemp = '_ABC' then
          deviceInfo.phase := 'ABC'
        else if strTemp = '_A' then
          deviceInfo.phase := 'A'
        else if strTemp = '_B' then
          deviceInfo.phase := 'B'
        else if strTemp = '_C' then
          deviceInfo.phase := 'C'
        else
          deviceInfo.phase := 'Error';
      end
      else
        deviceInfo.phase := 'Error';

      // Extract power
      pvd := FindVariableInEnt(pdev, 'Power');
      if pvd <> nil then
        deviceInfo.power := pdouble(pvd^.data.Addr.Instance)^
      else
        deviceInfo.power := -1;

      // Extract cos phi
      pvd := FindVariableInEnt(pdev, 'CosPHI');
      if pvd <> nil then
        deviceInfo.cosPhi := pdouble(pvd^.data.Addr.Instance)^
      else
        deviceInfo.cosPhi := -1;

      deviceInfo.typeKc := ''; // Not used in current implementation

      ExportDevice(deviceInfo);
    end;

    zcUI.TextMessage('Devices exported to Access database', TMWOHistoryOut);
  finally
    devices.Free;
  end;
end;

procedure TAccessDBExporter.ExportConnectionsFromDrawing(ADeviceCollector: TDeviceDataCollector);
var
  devices: specialize TVector<TDeviceData>;
  i, j: integer;
begin
  devices := ADeviceCollector.CollectAllDevices;

  try
    for i := 0 to devices.Size - 1 do
    begin
      for j := 0 to Length(devices[i].Connections) - 1 do
      begin
        ExportConnection(
          devices[i].DevName,
          devices[i].Connections[j].HeadDeviceName,
          devices[i].Connections[j].NGHeadDevice
        );
      end;
    end;

    zcUI.TextMessage('Connections exported to Access database', TMWOHistoryOut);
  finally
    devices.Free;
  end;
end;

end.
