import openpyxl



if __name__ == '__main__':

    wrkbk = openpyxl.load_workbook("Date_State.xlsx")

    sh = wrkbk.active


    entries = []

    # iterate through excel and display data
    # max_row = 5178
    for row in sh.iter_rows(min_row=2, min_col=1, max_row=3776, max_col=1):
        date = row[0].value
        date = date[2:5]
        date = date.replace("-", " ")
        entries.append(date)


    with open('days_extracted.txt', 'w') as f:
        for x in range(len(entries)):
            #print(x.__str__())
            f.write(entries[x])
            f.write('\n')

