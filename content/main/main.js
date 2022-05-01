    function arrayToTable(tableData) {
        var table = $('<table></table>');
        $(tableData).each(function (i, rowData) {
            var row = $('<tr></tr>');
            $(rowData).each(function (j, cellData) {
                row.append($('<td>'+cellData+'</td>'));
            });
            table.append(row);
        });
        return table;
    }

    $.ajax({
        type: "GET",
        url: "https://raw.githubusercontent.com/wenlab501/EpiTrans/main/content/functions.csv",
        success: function (data) {
			data=Papa.parse(data).data;

			for (var i = 1; i < data.length-1; i++) {
				row=data[i];

				let txt = '<tr>'+'<td><a href="'+row[1]+'.html"><button>'+row[1]+'</button></a></td>'+
								'<td>'+row[2]+'</td>'+
								'<td>'+row[3]+'</td>'+
						  '</tr>';

				$('tbody').append(txt);

			}
        }
    });
