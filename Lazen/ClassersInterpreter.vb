Public Class ClassersInterpreter
    Public Shared Sub start(line As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("classer") Then
            Dim classerName = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.Substring(7)))
            If Not Variables.ClasserExists(classerName.ToLower) Then

                Variables.CreateClasser(classerName.ToLower)

            Else
                'pup error cause a classer called 'classername' already exists
            End If
        End If
    End Sub
End Class