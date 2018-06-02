Public Class Returns
    Public Shared Function start(line As String, isFunction As Boolean, functionName As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("return ") Then

            If isFunction Then
                Dim nomDeLaFonction As String = functionName.ToLower
                Dim getReturn As String = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(7))


                Functions.listOfFunctionReturns.Items(Functions.listOfFunctionsNames.Items.IndexOf(nomDeLaFonction)) = getReturn

                Return "exit"
            Else

                'pup error cause a return should be on a function

            End If
        End If

            Return ""
    End Function
End Class