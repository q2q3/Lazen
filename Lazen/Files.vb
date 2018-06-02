Imports System.IO
Imports System.Security
Imports System.Security.Principal

Public Class Files
    Public Shared Function Start(realinput As String) As String
        If FormatConverters.removeSpacesAtBeginningAndEnd(realinput).ToLower.StartsWith("file") Then

            If realinput.Substring(4).StartsWith(".") Then
                If realinput.Substring(5).ToLower.StartsWith("info") Then
                    Dim prepareFileToAnalyze As String = FormatConverters.ConvertToAbleToRead(realinput.Substring(9))

                    If prepareFileToAnalyze.Contains("::") Then
                        Dim fileToAnalyze As String = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(prepareFileToAnalyze.Substring(0, prepareFileToAnalyze.LastIndexOf("::")))))
                        Dim getInformationToGet As String = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(prepareFileToAnalyze.Substring(prepareFileToAnalyze.LastIndexOf("::") + 2))))

                        If File.Exists(fileToAnalyze) Then

                            Dim fileInformations As FileInfo = New FileInfo(fileToAnalyze)
                            Dim fileVersionsInfo As FileVersionInfo = FileVersionInfo.GetVersionInfo(fileToAnalyze)

                            If getInformationToGet.ToLower = "creationdate" Then
                                Return File.GetCreationTime(fileToAnalyze)
                            ElseIf getInformationToGet.ToLower = "lastaccessdate" Then
                                Return File.GetLastAccessTime(fileToAnalyze)
                            ElseIf getInformationToGet.ToLower = "filename" Then
                                Return fileInformations.Name
                            ElseIf getInformationToGet.ToLower = "extension" Then
                                Return fileInformations.Extension
                            ElseIf getInformationToGet.ToLower = "directoryname" Then
                                Return fileInformations.DirectoryName
                            ElseIf getInformationToGet.ToLower = "filesize" Then
                                Return fileInformations.Length
                            ElseIf getInformationToGet.ToLower = "lastwritedate" Then
                                Return fileInformations.LastWriteTime
                            ElseIf getInformationToGet.ToLower = "readtext" Then
                                Return File.ReadAllText(fileToAnalyze)
                            ElseIf getInformationToGet.ToLower = "fullname" Then
                                Return fileInformations.FullName
                            ElseIf getInformationToGet.ToLower = "description" Then
                                Return fileVersionsInfo.FileDescription
                            ElseIf getInformationToGet.ToLower = "comments" Then
                                Return fileVersionsInfo.Comments
                            ElseIf getInformationToGet.ToLower = "companyname" Then
                                Return fileVersionsInfo.CompanyName
                            ElseIf getInformationToGet.ToLower = "version" Then
                                Return fileVersionsInfo.FileVersion
                            ElseIf getInformationToGet.ToLower = "internalname" Then
                                Return fileVersionsInfo.InternalName
                            ElseIf getInformationToGet.ToLower = "debugstate" Then
                                Return FormatConverters.convertTrueFalse(fileVersionsInfo.IsDebug)
                            ElseIf getInformationToGet.ToLower = "patchstate" Then
                                Return FormatConverters.convertTrueFalse(fileVersionsInfo.IsPatched)
                            ElseIf getInformationToGet.ToLower = "prereleasestate" Then
                                Return FormatConverters.convertTrueFalse(fileVersionsInfo.IsPreRelease)
                            ElseIf getInformationToGet.ToLower = "privatebuildstate" Then
                                Return FormatConverters.convertTrueFalse(fileVersionsInfo.IsPrivateBuild)
                            ElseIf getInformationToGet.ToLower = "specialbuildstate" Then
                                Return FormatConverters.convertTrueFalse(fileVersionsInfo.IsSpecialBuild)
                            ElseIf getInformationToGet.ToLower = "language" Then
                                Return fileVersionsInfo.Language.ToUpper
                            ElseIf getInformationToGet.ToLower = "copyright" Then
                                Return fileVersionsInfo.LegalCopyright
                            ElseIf getInformationToGet.ToLower = "trademarks" Then
                                Return fileVersionsInfo.LegalTrademarks
                            ElseIf getInformationToGet.ToLower = "originalfilename" Then
                                Return fileVersionsInfo.OriginalFilename
                            ElseIf getInformationToGet.ToLower = "productname" Then
                                Return fileVersionsInfo.ProductName
                            ElseIf getInformationToGet.ToLower = "productversion" Then
                                Return fileVersionsInfo.ProductVersion
                            ElseIf getInformationToGet.ToLower = "fileowner" Then

                                Try

                                    Dim di As New FileInfo(fileToAnalyze)
                                    Dim ds As AccessControl.FileSecurity = di.GetAccessControl()
                                    Dim owner As NTAccount = CType(ds.GetOwner(GetType(NTAccount)), NTAccount)
                                    Return owner.ToString

                                Catch ex As Exception
                                    'pup error cause an error occured while getting the file owner
                                    Exit Function
                                End Try

                            Else
                                'pup error cause getinformationtoget isn't an information type
                                Exit Function
                            End If
                        Else
                            'pup error cause filetoanalyze isn't existing
                            Exit Function
                        End If
                    Else
                        'pup error cause programmer have to define a file path and a type or information which is delimited by ::
                        Exit Function
                    End If
                End If
            Else

                If realinput.Substring(5).ToLower.StartsWith("getattributes") Then

                    Dim checkPointRealInput As String = realinput.Substring(5)
                    If checkPointRealInput.Substring(13).StartsWith(".") Then

                        Dim saveAtPoint As String = checkPointRealInput.Substring(14)

                        If saveAtPoint.ToLower.StartsWith("archivestate") Then

                            Dim getCurrentExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(12)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath As String = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)

                                If (attributes And FileAttributes.Archive) = FileAttributes.Archive Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If

                        ElseIf saveAtPoint.ToLower.StartsWith("compressedstate") Then

                            Dim getCurrentExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(15)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath As String = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)
                                If (attributes And FileAttributes.Compressed) = FileAttributes.Compressed Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If

                        ElseIf saveAtPoint.ToLower.StartsWith("hiddenstate") Then

                            Dim getCurrentExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(11)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath As String = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)

                                If (attributes And FileAttributes.Hidden) = FileAttributes.Hidden Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If

                        ElseIf saveAtPoint.ToLower.StartsWith("systemstate") Then

                            Dim getCurrentExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(11)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath As String = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)

                                If (attributes And FileAttributes.System) = FileAttributes.System Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If
                        ElseIf saveAtPoint.ToLower.StartsWith("readonlystate") Then

                            Dim getCurrentExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(13)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath As String = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)

                                If (attributes And FileAttributes.ReadOnly) = FileAttributes.ReadOnly Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If
                        ElseIf saveAtPoint.ToLower.StartsWith("encryptedstate") Then

                            Dim getCurrentExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(14)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)

                                If (attributes And FileAttributes.Encrypted) = FileAttributes.Encrypted Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If

                        ElseIf saveAtPoint.ToLower.StartsWith("temporarystate") Then
                            Dim getCurrentExpression = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(saveAtPoint.Substring(8)))

                            If File.Exists(FormatConverters.getExpression(getCurrentExpression)) Then

                                Dim FilePath As String = FormatConverters.getExpression(getCurrentExpression)
                                Dim attributes As FileAttributes = File.GetAttributes(FilePath)

                                If (attributes And FileAttributes.Temporary) = FileAttributes.Temporary Then
                                    Return "1"
                                Else
                                    Return "0"
                                End If

                            Else
                                'pup error cause getExpression(getCurrentExpression) (file) is not existing
                            End If
                        End If

                    Else
                        'pup error cause user have to specify an attribute at file.getattributes
                    End If

                End If
            End If
        End If
    End Function
End Class