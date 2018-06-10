Public Class GotoTickets
    Shared ListOfTickets As New ListBox
    Shared ListOfLines As New ListBox

    Public Shared Function Start(line As String) As String

        'goto_ticket:HELLO

        If FormatConverters.removeSpacesAtBeginningAndEnd(line.ToLower.StartsWith("goto_ticket:")) Then

            Dim ticketName As String = FormatConverters.removeSpacesAtBeginningAndEnd(line).Split(":")(1)
            Return LoadTicket(ticketName)

        End If

        Return ""
    End Function
    Public Shared Function LoadTicket(ticketname As String) As String
        If ListOfTickets.Items.Contains(ticketname) Then

            Return ListOfLines.Items(ListOfTickets.Items.IndexOf(ticketname))

        Else
            'pup error cause ticket 'ticketname' isn't existing
            Return "error"
        End If
    End Function
    Public Shared Sub IdentifyTickets(code As String)
        Dim counter As Long = 0
        For Each i As String In code.Split(ControlChars.Lf)
            'ticket:hello
            If i.ToLower.StartsWith("ticket:") Then
                Dim getTicketName As String = i.Split(":")(1)

                If CharacterVerification.Verify(getTicketName) = "ok" Then
                    ListOfTickets.Items.Add(getTicketName)
                    ListOfLines.Items.Add(counter)
                Else
                    'pup error cause invalid character in ticket name : CharacterVerification.Verify(getTicketName).Split("-")(1)
                End If

            End If

            counter += 1
        Next
    End Sub
End Class