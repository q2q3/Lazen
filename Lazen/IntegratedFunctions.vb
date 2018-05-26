Imports System.Globalization

Public Class IntegratedFunctions
    Public Shared Function getFunctionCall(input As String) As String
        Dim realInput = input
        If realInput.StartsWith("&") Then
            realInput = realInput.Substring(1)
        End If
        If realInput.ToLower.StartsWith("math.get") Then
            Dim getMathematicalExpression = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(realInput.Substring(8)))
            Dim FinalResultOfMathematicalExpression = ""
            For Each i As String In getMathematicalExpression.Split(" ")
                If i = "+" Or i = "-" Or i = "/" Or i = "|" Or i = "%" Or i = "*" Then
                    FinalResultOfMathematicalExpression += i.Replace("|", "/") & " "
                Else
                    FinalResultOfMathematicalExpression += FormatConverters.getExpression(i) & " "
                End If
            Next
            Dim tables As New DataTable
            Dim EquationResult = tables.Compute(FinalResultOfMathematicalExpression, " ")
            Return EquationResult
        ElseIf realInput.ToLower.StartsWith("math.computeexpression") Then
            Dim getMathematicalExpression = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(realInput.Substring(22)))).Replace("|", "/")
            Dim tables As New DataTable
            Dim EquationResult = tables.Compute(getMathematicalExpression, " ")
            Return EquationResult
        ElseIf realInput.ToLower.StartsWith("file") Then
            Return Files.Start(realInput)
        ElseIf realInput.ToLower.StartsWith("directory") Then

        ElseIf realInput.ToLower.StartsWith("system") Then
            'informations composants/drivers
            'cryptographie
            'informations os/windows
        ElseIf realInput.ToLower.StartsWith("string") Then
            'modifier des strings
            'tolower, toupper, etc..
        ElseIf realInput.ToLower.StartsWith("network") Then
            'ping
        ElseIf realInput.ToLower.StartsWith("date") Then
            If realInput.ToLower.Substring(4).StartsWith(".") Then
                'MsgBox("object: " & New DateTime(1970, 1, 1))
                If realInput.ToLower.Substring(5).ToLower.StartsWith("timeofday") Then
                    Dim GetExpressionOfDate = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(realInput.ToLower.Substring(5).Substring(9))))
                    If GetExpressionOfDate.Replace(" ", "") = "" Then
                        Return Date.Now.ToString.Split(" ")(1)
                    Else
                        Dim timestamp = DateTime.UtcNow
                        Dim replaceBro = GetExpressionOfDate.Replace("[hh]", timestamp.ToString("HH", CultureInfo.InvariantCulture)) _
                            .Replace("[mm]", timestamp.ToString("mm", CultureInfo.InvariantCulture)) _
                            .Replace("[ss]", timestamp.ToString("ss", CultureInfo.InvariantCulture)) _
                            .Replace("[ms]", timestamp.ToString("fff", CultureInfo.InvariantCulture))
                        Return replaceBro
                    End If
                ElseIf realInput.ToLower.Substring(5).ToLower.StartsWith("today") Then
                    Dim GetExpressionOfDate = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(realInput.ToLower.Substring(5).Substring(5))))
                    If GetExpressionOfDate.Replace(" ", "") = "" Then
                        Return Date.Today
                    Else
                        Dim timestamp = Date.Today
                        Dim replaceBro = GetExpressionOfDate.Replace("[day]", timestamp.ToString.Split("/")(0)) _
                            .Replace("[month]", timestamp.ToString.Split("/")(1)) _
                            .Replace("[year]", timestamp.ToString.Split("/")(2).Split(" ")(0))
                        Return replaceBro
                    End If
                End If
            ElseIf realInput.ToLower.Substring(5).ToLower.StartsWith("dayofweek") Then
                Dim datetime As New DateTime
                '  ElseIf realInput.ToLower.Substring(5).ToLower.StartsWith("now") Then
                ' MsgBox("datetime: " & Date.Now)
                '  Return Date.Now
            End If
            'dayofweek à faire en fonction lazen
            'daysinmonth à faire en fonction lazen
            'leapyearstate
        End If
    End Function
End Class