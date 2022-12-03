import openpyxl
import re

class Entry:

  def __init__(self, date, state):
    self.date = date
    self.state = state
    self.observations = 0

  def __str__(self):
      return self.date + ", " + self.state + ", " + str(self.observations) + ", "




class EntryHolder:

    entryList = []

    def exists(self, entry):
        for x in range(len(self.entryList)):
            if self.entryList[x].date == entry.date and self.entryList[x].state == entry.state:
                self.entryList[x].observations = self.entryList[x].observations + 1
                #print("added new observation")
                return
        self.__addEntry(entry)
        #print("No entry found, added new")

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
    for row in sh.iter_rows(min_row=2, min_col=1, max_row=5178, max_col=2):
        date = row[0].value
        state = row[1].value
        e = Entry(date, state)
        #print("Entry: " + date + ", " + state)
        entryHolder.exists(e)

    print("finished counting/sorting, writing to text file")
    with open('date_state_observations.txt', 'w') as f:
        for x in entryHolder.getEntries():
            #print(x.__str__())
            f.write(x.__str__())
            f.write('\n')
