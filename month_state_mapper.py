import openpyxl
import re

class Entry:

  def __init__(self, month, state):
    self.month = month
    self.state = state
    self.observations = 0

  def __str__(self):
      return self.month + ", " + self.state + ", " + str(self.observations) + ", "




class EntryHolder:

    entryList = []

    def exists_depr(self, entry):
        for x in range(len(self.entryList)):
            if self.entryList[x].date == entry.date and self.entryList[x].state == entry.state:
                self.entryList[x].observations = self.entryList[x].observations + 1
                print("added new observation")
                return
        self.__addEntry(entry)
        print("No entry found, added new")

    def exists(self, entry):
        for x in range(len(self.entryList)):
            if self.entryList[x].month == entry.month and self.entryList[x].state == entry.state:
                self.entryList[x].observations = self.entryList[x].observations + 1
                print("added new observation")
                return
        self.__addEntry(entry)

    def __addEntry(self, entry):
        self.entryList.append(entry)

    def getEntries(self):
        return self.entryList




if __name__ == '__main__':

    # load excel with its path
    wrkbk = openpyxl.load_workbook("Date_State.xlsx")

    sh = wrkbk.active

    entryHolder = EntryHolder()

    # iterate through excel and display data
    #max_row = 5178
    for row in sh.iter_rows(min_row=2, min_col=1, max_row=5177, max_col=2):
        date = row[0].value
        if date == None:
            continue
        date = date[0:2]
        date = date.replace("-", " ")
        state = row[1].value
        e = Entry(date, state)
        print(date + " - " +  state)
        entryHolder.exists(e)

    print("finished counting/sorting, writing to text file")
    with open('monthly_state_observations.txt', 'w') as f:
        for x in entryHolder.getEntries():
            #print(x.__str__())
            f.write(x.__str__())
            f.write('\n')

