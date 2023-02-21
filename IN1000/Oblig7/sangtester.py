from sang import Sang

def hovedprogram():
    sang1 = Sang("Lady Gaga and Bradley Cooper", "Shallow")
    
    # Metoden spill
    print("Spiller av test-objekt:")
    sang1.spill()
    
    print("Tester sjekkArtist med 'Lady Gaga and Bradley Cooper'")	# True
    assert(sang1.sjekkArtist("Lady Gaga and Bradley Cooper"))     	
    print("Tester sjekkArtist med 'Lord Gaga'") 					# True, ett ord finnes i artistnavnet
    assert(sang1.sjekkArtist("Lord Gaga"))        
    print("Tester sjekkArtist med 'Sadley'")						# False
    assert(not sang1.sjekkArtist("Sadley"))       
    print("Tester sjekkArtist med 'Sadley'")	
    assert(not sang1.sjekkArtist("a"))                              # False
    print("Tester sjekkArtist med 'a'")	

    
    # Metoden sjekkTittel
    print("Tester sjekkTittel med 'Shallow'")						# True
    assert(sang1.sjekkTittel("Shallow"))          
    print("Tester sjekkTittel med 'shalLow'")						# True
    assert(sang1.sjekkTittel("shalLow"))          
    print("Tester sjekkTittel med 'Hallow'")						# False
    assert(not sang1.sjekkTittel("Hallow"))       
    print()
 
    # Metoden sjekkArtistOgTittel
    print("Tester sjekkArtistOgTittel med 'Bradley Cooper' og 'Shallow'")	# True
    assert(sang1.sjekkArtistOgTittel('Bradley Cooper', 'Shallow')) 
    print("Tester sjekkArtistOgTittel med 'Booper' og 'Shallow'")			# False
    assert(not sang1.sjekkArtistOgTittel('Booper', 'Shallow')) 
    
hovedprogram()