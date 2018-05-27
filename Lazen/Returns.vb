Public Class Returns
    Public Shared Function start(line As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("return ") Then

            For Each i As String In Functions.listOfLines.Items
                If i.Split("-")(0) = Interpret.lineBoostCopy Then

                    Dim nomDeLaFonction As String = i.Split("-")(1).ToLower
                    Dim getReturn As String = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(7))

                    Interpret.lineBoostCopy = 0

                    Functions.listOfFunctionReturns.Items(Functions.listOfFunctionsNames.Items.IndexOf(nomDeLaFonction)) = getReturn

                    Return "exit"

                End If

            Next

        End If

        Return ""
    End Function
End Class