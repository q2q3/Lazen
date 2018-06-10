Public Class CharacterVerification
    Public Shared Function Verify(input As String) As String
        'cette fonction vérifie si l'input contient uniquement les caractères 
        'du charmap précisé
        Dim blacklisted As String = "$&#, .µ""{(></\)}=:"

        For Each i As String In blacklisted
            If input.Contains(i) Then
                Return "no-" & i
            End If
        Next

        Return "ok"
    End Function
End Class