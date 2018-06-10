Public Class ClassersInterpreter
    Public Shared Sub start(line As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("classer ") Then

            Dim classerName As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.Substring(7)))

            If CharacterVerification.Verify(classerName) <> "ok" Then
                'pup error cause invalid characters in voidname : CharacterVerification.Verify(classerName).Split("-")(1)
                Exit Sub
            End If


            If Not Variables.ClasserExists(classerName.ToLower) Then
                Variables.CreateClasser(classerName.ToLower)
            Else
                'pup error cause a classer called 'classername' already exists
            End If

        End If
    End Sub
End Class