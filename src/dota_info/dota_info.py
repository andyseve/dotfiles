# !/run/current-system/sw/bin/python3
from bs4 import BeautifulSoup
import urllib.request
import sys, getopt

class HeroWikiUrl:
    def __init__(self, name):
        self.name = self.convert(name)
        self.url = "https://dota2.gamepedia.com/" + self.name
        self.data = urllib.request.urlopen(self.url).read()

    def convert(self, name):
        return name.title().replace(" ", "_")

class cstrs:
    red   = "\033[0;31m"  
    blue  = "\033[0;34m"
    green = "\033[0;32m"
    reset = "\033[0;0m"
    bold    = "\033[;1m"
    reverse = "\033[;7m"

class HeroInfo:
    def __init__(self, name):
        self.wiki = HeroWikiUrl(name)
        self.soup = BeautifulSoup(self.wiki.data, 'html.parser')

        # The infobox
        self.infobox = self.soup.find('table', attrs={'class':'infobox'}).tbody
        self.primaryAttribute = self.infobox.find('div', attrs={'id':'primaryAttribute'}).a.get('title')
        if self.primaryAttribute == "Strength":
            self.primaryAttribute = cstrs.red + self.primaryAttribute + cstrs.reset
        elif self.primaryAttribute == "Agility":
            self.primaryAttribute = cstrs.green + self.primaryAttribute + cstrs.reset
        else:
            self.primaryAttribute = cstrs.blue + self.primaryAttribute + cstrs.reset

        self.attrDivs = self.infobox.find_all('div')
        self.strength = self.attrDivs[-3].get_text()
        self.agility = self.attrDivs[-2].get_text()
        self.intelligence = self.attrDivs[-1].get_text()

        self.infoboxRows = self.infobox.find_all('tr')
        self.values = [[x.th.get_text(), x.td.get_text()] for x in self.infoboxRows[-13:]]
        for i in self.values:
            i[0] = i[0][:-1]
            i[1] = i[1][:-1]
        self.values[1][0] = self.values[1][0][7:]
        self.values[2][0] = self.values[2][0][7:]

        self.advStatsTitles = [ [ row.th.text[:-1]] for row in self.infoboxRows[-21:-14]]
        titleLen = max([len(row[0]) for row in self.advStatsTitles])
        for title in self.advStatsTitles:
            title[0] = " " * (titleLen - len(title[0])) + title[0]

        self.advStatsValues = [ [ col.text[:-1] for col in row.find_all('td') ] for row in self.infoboxRows[-21:-14] ]
        self.advStatsTable = [ a + b for (a,b) in zip(self.advStatsTitles, self.advStatsValues) ]

        # Hero Bio
        self.bio = self.soup.find('div', attrs={'id':'heroBio'})
        self.bioDiv = self.bio.find_all('div')
        self.disc = self.bioDiv[0].span.text
        self.intro = self.bioDiv[1].text.split(" ", 1)[1]
        self.voice = self.bioDiv[-1].a.text        
        self.lore = self.bioDiv[5].text

        self.abilities = self.soup.find_all('div', attrs={'class':'ability-background'})
        self.abilityNotes = [ ability.find_next_sibling('div').get_text() for ability in self.abilities ]
        
        # self.talentTable


    def add_change(self, orig, chg):
        ret = orig.replace("(",cstrs.blue + "(" + chg + " ")
        ret = ret.replace(")", ")" + cstrs.reset)
        return ret

    def abilityName(self, abil):
        return abil.find_all('div')[1].text.split(" Link",1)[0]

    def abilityInfo(self, abil):
        ret = ""
        divs = abil.find_all('div')
        ret += cstrs.bold + cstrs.red + divs[1].text.split(" Link",1)[0] + cstrs.reset + "\n"
        ret += "Ability: " + divs[9].a.text + "\n"
        affectsstr = divs[10].text.split("Affects",1)
        if len(affectsstr) > 1: ret += "Affects: " + affectsstr[1] + "\n"
        damagestr = divs[11].text.split("Affects",1)
        if len(damagestr) > 1: ret += "Damage: " + damagestr[1] + "\n"
        ret += "\n"


        infodiv = divs[13]
        infodivs = infodiv.find_all('div')
        mode = 0
        for div in infodivs:
            tmp = div.text.replace("\n","")
            if tmp == "Modifiers":
                moddivs = div.find_next_sibling('div').find_all('div')
                if len(moddivs):
                    ret += cstrs.blue + "Modifiers:" + cstrs.reset + "\n"
                for idiv in moddivs:
                    ret += idiv.text + "\n"
                break
            if tmp == "" or tmp is None:
                continue
            if div.a is None:
                ret += tmp + "\n"
            else:
                cur = div.a['href'].split("/",1)[1].replace("_"," ")
                if cur == "Talents" and mode == 0:
                    ret += self.add_change(tmp, "Talent") + "\n"
                elif cur == "Aghanim%27s Scepter":
                    if mode == 0:
                        ret += self.add_change(tmp, "Aghs") + "\n"
                    else:
                        ret += cstrs.blue + "Aghanim's Scepter: " + cstrs.reset + tmp + "\n"
                else:
                    ret += cstrs.blue + cur + ": " + cstrs.reset + tmp + "\n"
                if cur == "Mana":
                    mode = 1
                    ret += "\n"

        return ret


    def name(self):
        ret = ""
        ret += "Hero: " + self.disc + "\n"
        ret += "Primary Attribute: " + cstrs.bold + self.primaryAttribute + cstrs.reset + "\n"
        ret += cstrs.red + "Str: " + self.strength + cstrs.reset + "\t"
        ret += cstrs.green + "Agi: " + self.agility + cstrs.reset + "\t"
        ret += cstrs.blue + "Int: " + self.intelligence + cstrs.reset
        return ret

    def hero(self):
        ret = ""
        ret += cstrs.bold + self.intro + cstrs.reset + "\n"
        ret += self.lore + "\n"
        return ret

    def stats(self):
        ret = ""
        for row in self.values:
            tmp = " " * (16 - len(row[0]))
            tmp += row[0] + ": "
            # while len(tmp) <= 18: tmp = tmp + " "
            ret += tmp + "\t" + row[1] + "\n"
        return ret

    def advStats(self):
        ret = ""
        for row in self.advStatsTable:
            for val in row:
                ret += val + "\t"
            ret += "\n"
        return ret

    def talents(self):
        ret = ""
        return ret

    def allAbilities(self):
        ret = ""
        for abil in self.abilities:
            ret += self.abilityInfo(abil) + "\n"
        return ret

    def full(self):
        ret = ""
        ret += cstrs.bold + "Stats:" + cstrs.reset + "\n"
        ret += self.stats()
        ret += self.advStats()
        ret += cstrs.bold + "Lore:" + cstrs.reset + "\n"
        ret += self.hero()
        ret += cstrs.bold + "Abilities:" + cstrs.reset + "\n"
        ret += self.allAbilities()
        ret += cstrs.bold + "Talents:"  + cstrs.reset + "\n"
        return ret

def option_discription(shortopt, longopt, disc):
    ret = "  -" + shortopt
    ret += " " * (6 - len(ret))
    ret += "--" + longopt
    ret += " " * (20 - len(ret))
    ret += disc + "\n"
    return ret

def usage():
    ret = "Usage: dota_info [OPTION]... [hero name]\n"
    ret += option_discription("h", "help", "print this message")
    ret += option_discription("s", "stats", "show hero stats")
    ret += option_discription("a", "abilities", "show ability information")
    ret += option_discription("t", "talents", "show talent information")
    ret += option_discription("f", "help", "show everything")
    return ret

if __name__ == "__main__":
    inp = sys.argv[1:]
    try:
        opts, args = getopt.getopt(inp, "hsaft", ["help", "full", "stats", "abilities", "talents"])
    except getopt.GetoptError:
        print(usage())
        sys.exit(2)
    name = ' '.join(args)
    hero = HeroInfo(name)
    print(hero.name())
    if len(opts) == 0:
        print(hero.stats())
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            print(usage())
        elif opt in ('-f', '--full'):
            print(hero.full())
            sys.exit(0)
        elif opt in ('-s', '--stats'):
            print(hero.stats())
        elif opt in ('-a', '--abilities'):
            print(hero.allAbilities())
        elif opt in ('-t', '--talents'):
            print(hero.talents())

