Imports System.Globalization

Public Class IntegratedFunctions
    Public Shared Function getFunctionCall(input As String) As String

        Dim realInput As String = input

        If realInput.StartsWith("&") Then
            realInput = realInput.Substring(1)
        End If

        If realInput.ToLower.StartsWith("math.get") Then

            Dim getMathematicalExpression As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(realInput.Substring(8)))
            Dim FinalResultOfMathematicalExpression As String = ""

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

            Dim getMathematicalExpression As String = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(realInput.Substring(22)))).Replace("|", "/")
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

                If realInput.ToLower.Substring(5).ToLower.StartsWith("timeofday") Then

                    Dim GetExpressionOfDate As String = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(realInput.ToLower.Substring(5).Substring(9))))

                    If GetExpressionOfDate.Replace(" ", "") = "" Then

                        Return Date.Now.ToString.Split(" ")(1)

                    Else

                        Dim timestamp As Date = DateTime.UtcNow

                        Dim replaceBro As String = GetExpressionOfDate.Replace("[hh]", timestamp.ToString("HH", CultureInfo.InvariantCulture)) _
                            .Replace("[mm]", timestamp.ToString("mm", CultureInfo.InvariantCulture)) _
                            .Replace("[ss]", timestamp.ToString("ss", CultureInfo.InvariantCulture)) _
                            .Replace("[ms]", timestamp.ToString("fff", CultureInfo.InvariantCulture))

                        Return replaceBro

                    End If

                ElseIf realInput.ToLower.Substring(5).ToLower.StartsWith("today") Then

                    Dim GetExpressionOfDate As String = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(realInput.ToLower.Substring(5).Substring(5))))

                    If GetExpressionOfDate.Replace(" ", "") = "" Then

                        Return Date.Today

                    Else

                        Dim timestamp As Date = Date.Today
                        Dim replaceBro As String = GetExpressionOfDate.Replace("[day]", timestamp.ToString.Split("/")(0)) _
                            .Replace("[month]", timestamp.ToString.Split("/")(1)) _
                            .Replace("[year]", timestamp.ToString.Split("/")(2).Split(" ")(0))

                        Return replaceBro

                    End If
                End If

            ElseIf realInput.ToLower.Substring(5).ToLower.StartsWith("dayofweek") Then

                Dim datetime As New DateTime
                'in development

            End If

            'to do : 

            '- dayOfWeek
            '- daysInMonth
            '- leapYearState

        End If
    End Function
End Class