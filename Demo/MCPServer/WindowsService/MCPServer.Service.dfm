object ServiceModule: TServiceModule
  OnCreate = ServiceCreate
  DisplayName = 'ServiceModule'
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  Height = 480
  Width = 640
end
