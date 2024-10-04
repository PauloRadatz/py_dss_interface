/* DelphiSets.h ====================================================================================

	Dan's Substitute Sets ** (c) Copyright 2011, Daniel Flower
	A high-performance template class that emulates Delphi's sets.

	Authorized for unlimited use in any Delphi2Cpp project.

	Features:
	- Binary compatibility with Delphi's sets.
	- All Delphi and most CBuilder operators and features.
	- Supports unlimited elements (vs Delphi's limit of 256).
	- Highly optimized (supports enhanced CPU instruction sets).
	- Configurable element datatype, thus 8/16/32/64bit portability.

	Differences:
	- The Delphi 'in' operator is now 'Contains', like CBuilder.
	- The size of the set in memory is fixed, unlike Delphi's sets.
	  See the binary compatibility functions at the end of the template.
	- Each set is considered a different type, so macros are provided
	  to assist with operations between different sets.


Example TSet declarations:
In Delphi: TAllChars = Set of Char;
   In C++: typedef TSet<char, 0, 255> TAllChars;

In Delphi: TAlphabet = Set of 'a' .. 'z';
   In C++: typedef TSet<char, 'a', 'z'> TAlphabet;

In Delphi: TDays = ( Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday );
           TWorkdays = Set of Monday .. Friday;
   In C++: enum TDays { Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday };
           typedef TSet<TDays, Monday, Friday> TAlphabet;

==================================================================================================*/


// Configuration Section ===========================================================================

// Change type of TSetElement as required, however, performance is best when the element size is 
// the same as the native register size. Thus, 32bit integers are ideal for 32bit executables.
typedef	unsigned int	TSetElement;		// Must be unsigned!

// TSETLIMIT defines maximum number of bits to support in a set. Delphi only 
// supported 256 bits, but you can use as few or as many as you require.
#define	TSETLIMIT	(256)					// Must be a multiple of TSETBITS.

// TSETPOWER defines where to split a set index into element and sub-element indexes.
// This must be coordinated with TSetElement's datatype: 8bit = 3, 16bit = 4, 32bit = 5, 64bit = 6.
#define	TSETPOWER	(5)						// Exponent of 2. Example: 2 ^ (5) = 32 for 32bit ints.

// Don' modify these...
#define	TSETBITS	(1 << TSETPOWER)		// Number of bits per element.
#define	TSETPMASK	(TSETBITS - 1)			// Sub-element bit mask.
#define	TSETINTS	(TSETLIMIT >> TSETPOWER)// Number of elements in the set.


// Shortcuts & Macros ==============================================================================

// Return a typeless set (for operations between sets of dissimilar types).
#define AsSet(Set)					((Set).RawBits())

// Substitute Add/Sub/MulSets where sets are added/subtracted/multiplied together: 
// In Delphi: Output := Apples + Oranges;
//    In C++: Output = AddSets(Apples, Oranges);
#define AddSets(Set1, Set2)			AsSet((Set1) + AsSet(Set2))
#define SubSets(Set1, Set2)			AsSet((Set1) - AsSet(Set2))
#define MulSets(Set1, Set2)			AsSet((Set1) * AsSet(Set2))

// Note: Any number of sets can be added together without the AddSet macro like so:
// In Delphi: AtoZ := AtoF + GtoQ + RtoZ;
//    In C++: AtoZ := AsSet( AsSet(AtoF) + AsSet(GtoQ) + AsSet(RtoZ) );

// Substitute SubsetOf/SupersetOf where appropriate:
// In Delphi: if (Weekends <= Weekdays) or (ThisMonth >= Weekdays) then
//    In C++: if (SubsetOf(Weekends, Weekdays) || SupersetOf(ThisMonth, Weekdays))
#define SubsetOf(SetIs, SetOf)		(SetIs <= AsSet(SetOf))
#define SupersetOf(SetIs, SetOf)	(SetIs >= AsSet(SetOf))


// TSet Template Class =============================================================================

typedef TSetElement		TSetBits[TSETINTS];

template <typename Type, Type Low, Type High> class TSet {
private:
	TSetBits bits;

public:
	// Constructors and Support Functions =========================================================

	// Default constructor.
	// In Delphi: CharSet : Set of Char;
	//    In C++: typedef TSet<char, 0, 255> TCharSet;
	TSet() {
		Clear();
	}

	// Copy constructor (for dissimilar set types).
	// In Delphi: LowerCase : Set of 'a' .. 'z';
	//            UpperCase : Set of 'A' .. 'Z';
	//            LowerCase := UpperCase;
	//    In C++: TSet<char, 'a', 'z'> LowerCase;
	//            TSet<char, 'A', 'Z'> UpperCase;
	//            LowerCase = UpperCase.RawBits();			// See the AsSet() macro.
	TSet(const TSetBits &RawBits) {
		for (int i = 0; i < TSETINTS; bits[i] = RawBits[i], i++);
	}
	
	// Return the raw bits indirectly (for dissimilar set types).
	// In Delphi: TMixed = Set of Char;
	//            TLetters = Set of 'a' .. 'z'
	//            TNumbers = Set of '0' .. '9';
	//            Mixed := Letters + Numbers;
	//    In C++: typedef TSet<char, 0, 255> TMixed;
	//            typedef TSet<char, 'a', 'z'> TLetter;
	//            typedef TSet<char, 'a', 'z'> TLetter;
	//            Mixed = (Letters + Numbers.RawBits()).RawBits();	// See the AddSets() macro.
	const TSetBits& RawBits() const {
		return bits;
	}

	// Empty this set completely.
	// In Delphi: CharSet := [];
	//    In C++: CharSet.Clear();
	TSet& Clear() {
		for (int i = 0; i < TSETINTS; bits[i++] = 0);
		return *this;
	}


	// Inclusion and Exclusion Functions ==========================================================

	// Add a single element to the set.
	// In Delphi: Include(MySet, 'x');
	//        or: MySet := MySet + ['x'];
	//    In C++: MySet.Include('x');
	inline void Include(const Type Index) {
		const TSetElement index  = (TSetElement)Index;
		//_ASSERTE(index >= 0 && index < TSETLIMIT);			// Assertion version of the safeguard below.
		if (index >= 0 && index < TSETLIMIT)					// Remark this line for better performance.
			bits[index >> TSETPOWER] |= (TSetElement)1 << (index & TSETPMASK);
	}

	// Add a range of elements to the set.
	// In Delphi: Include(MySet, 'x' .. 'z');
	//        or: MySet := MySet + ['x' .. 'z'];
	//    In C++: MySet.Include('x', 'z');
	void Include(const Type xLow, const Type xHigh) {
		const TSetElement low = (TSetElement)xLow, high = (TSetElement)xHigh;
		//_ASSERTE(low <= high && low >= 0 && high < TSETLIMIT);// Assertion version of the safeguard below.
		if (low <= high && low >= 0 && high < TSETLIMIT)		// Remark this line for better performance.
			for (unsigned int i = 0, o = (unsigned int)low >> TSETPOWER, 
				 b = (unsigned int)low & TSETPMASK; i <= (unsigned int)(high - low); i++) {
				bits[o] |= (TSetElement)1 << b; if (++b < TSETBITS) continue; o++; b = 0; }
	}

	// Chainable inclusion operator (for unordered inclusions).
	// In Delphi: Include(MySet, 'x');
	//        or: MySet := MySet + ['z', 'x', 'y'];
	//    In C++: MySet << 'x';
	//        or: MySet << 'z' << 'x' << 'y';
	TSet& operator << (const Type Index) {
		Include(Index);
		return *this;
	}


	// Remove a single element from a set.
	// In Delphi: Exclude(MySet, 'x');
	//        or: MySet := MySet - ['x'];
	//    In C++: MySet.Exclude('x');
	inline void Exclude(const Type Index) {
		const TSetElement index  = (TSetElement)Index;
		//_ASSERTE(index >= 0 && index < TSETLIMIT);			// Assertion version of the safeguard below.
		if (index >= 0 && index < TSETLIMIT)					// Remark this line for better performance.
			bits[index >> TSETPOWER] &= ~((TSetElement)1 << (index & TSETPMASK));
	}

	// Remove a range of elements from the set.
	// In Delphi: MySet := MySet - ['x' .. 'z'];
	//    In C++: MySet.Exclude('x', 'z');
	void Exclude(const Type xLow, const Type xHigh) {
		const TSetElement low = (TSetElement)xLow, high = (TSetElement)xHigh;
		//_ASSERTE(low <= high && low >= 0 && high < TSETLIMIT);// Assertion version of the safeguard below.
		if (low <= high && low >= 0 && high < TSETLIMIT)		// Remark this line for better performance.
			for (unsigned int i = 0, o = (unsigned int)low >> TSETPOWER, 
				 b = (unsigned int)low & TSETPMASK; i <= (unsigned int)(high - low); i++) {
				bits[o] &= ~((TSetElement)1 << b); if (++b < TSETBITS) continue; o++; b = 0; }
	}

	// Chainable exclusion operator (for unordered exclusions).
	// In Delphi: Exclude(MySet, 'x');
	//        or: MySet := MySet - ['z', 'x', 'y'];
	//    In C++: MySet >> 'x';
	//        or: MySet >> 'z' >> 'x' >> 'y';
	TSet& operator >> (const Type Index) {
		Exclude(Index);
		return *this;
	}


	// Set Operators ==============================================================================

	// Note: The = operator is automatically provided for by the classes default copy constructor,
	//       and the TSet(TSetBits) constructor does the same task for dissimilar set types.

	// Returns true when the set contains no elements, or false otherwise.
	bool Empty() const {
		TSetElement sum = 0;
		for (int i = 0; i < TSETINTS; sum += bits[i++]);
		return (sum) ? false : true;
	}

	// Returns true when Index is a member of the set, or false otherwise.
	// In Delphi: if 'x' in CharSet then
	//    In C++: if (CharSet.Contains('x'))
	inline bool Contains(const Type Index) const {
		const TSetElement index  = (TSetElement)Index;
		//_ASSERTE(index >= 0 && index < TSETLIMIT);			// Assertion version of the safeguard below.
		if (index < 0 || index >= TSETLIMIT) return false;		// Remark this line for better performance.
		return ((bits[index >> TSETPOWER] & ((TSetElement)1 << (index & TSETPMASK)))) ? true : false;
	}

	// Add another set to this one and return a new set containing their union.
	// In Delphi: WeekdaySet := WorkdaySet + WeekendSet;
	//    In C++: WeekdaySet = AddSets(WorkdaySet, WeekendSet);
	//        or: CurrentSet = LastSet + NextSet;				// Presuming they all use the same template...
	TSet operator + (const TSet &AddSet) const {
		TSet NewSet = *this;
		for (int i = 0; i < TSETINTS; NewSet.bits[i] |= AddSet.bits[i], i++);
		return NewSet;
	}

	// Add a set to this one, then return this (altered) set.
	// In Delphi: WorkdaySet := WorkdaySet + WeekendSet;		// No "actual" Delphi usage.
	//    In C++: WorkdaySet += AsSet(WeekendSet);
	//        or: CurrentSet += NextSet;						// Presuming they all use the same template...
	TSet& operator += (const TSet &AddSet) {
		for (int i = 0; i < TSETINTS; bits[i] |= AddSet.bits[i], i++);
		return *this;
	}

	// Return a copy of this set but with SubSet's items removed.
	// In Delphi: WeekdaySet := WorkdaySet - WeekendSet;
	//    In C++: WeekdaySet = SubSets(WorkdaySet, WeekendSet);
	TSet operator - (const TSet &SubSet) const {
		TSet NewSet = *this;
		for (int i = 0; i < TSETINTS; NewSet.bits[i] &= ~SubSet.bits[i], i++);
		return NewSet;
	}

	// Subtract a set from this one, then return this (altered) set.
	// In Delphi: WorkdaySet := WorkdaySet - WeekendSet;		// No "actual" Delphi usage.
	//    In C++: WorkdaySet -= AsSet(WeekendSet);
	//        or: CurrentSet -= LastSet;						// Presuming they all use the same template...
	TSet& operator -= (const TSet &SubSet) {
		for (int i = 0; i < TSETINTS; bits[i] &= ~SubSet.bits[i], i++);
		return *this;
	}

	// Return a new "intersection" set containing only the matches between this set and MulSet.
	// In Delphi: WeekdaySet := WorkdaySet * WeekendSet;
	//    In C++: WeekdaySet = MulSets(WorkdaySet, WeekendSet);
	TSet operator * (const TSet &MulSet) const {
		TSet NewSet = *this;
		for (int i = 0; i < TSETINTS; NewSet.bits[i] &= MulSet.bits[i], i++);
		return NewSet;
	}

	// Intersect this set with MulSet, then return this (altered) set.
	// In Delphi: WorkdaySet := WorkdaySet * WeekendSet;	// No "actual" Delphi usage.
	//    In C++: WorkdaySet *= AsSet(WeekendSet);
	//        or: CurrentSet *= LastSet;					// Presuming they all use the same template...
	TSet& operator *= (const TSet &MulSet) {
		for (int i = 0; i < TSETINTS; bits[i] &= ~MulSet.bits[i], i++);
		return *this;
	}

	// Return true if this set matches CmpSet exactly, or false if not.
	// In Delphi: if AppleSet = OrangeSet then
	//    In C++: if (AppleSet == OrangeSet)
	bool operator == (const TSet &CmpSet) const {
		TSetElement sum = 0;
		for (int i = 0; i < TSETINTS; sum |= bits[i] ^ CmpSet.bits[i], i++);
		return (sum) ? false : true;
	}

	// Return true if this set doesn't matche CmpSet exactly, or false if not.
	// In Delphi: if AppleSet <> OrangeSet then
	//    In C++: if (AppleSet != OrangeSet)
	bool operator != (const TSet &CmpSet) const {
		return !(*this == CmpSet);
	}

	// Return true if this set is completely contained within CmpSet, or false if not.
	// In Delphi: if AppleSet <= OrangeSet then
	//    In C++: if (AppleSet <= OrangeSet)
	bool operator <= (const TSet &CmpSet) const {
		TSetElement sum = 0;
		for (int i = 0; i < TSETINTS; sum |= bits[i] & ~CmpSet.bits[i], i++);
		return (sum) ? false : true;
	}

	// Return true if CmpSet is completely contained within this set, or false if not.
	// In Delphi: if AppleSet >= OrangeSet then
	//    In C++: if (AppleSet >= OrangeSet)
	bool operator >= (const TSet &CmpSet) const {
		return (CmpSet <= *this);
	}


	// Aliases for manipulating dissimilar templates ==============================================

	TSet operator + (const TSetBits &RawBits) const {
		return (*this + *(TSet*)&RawBits);
	}

	TSet& operator += (const TSetBits &RawBits) {
		return (*this += *(TSet*)&RawBits);
	}

	TSet operator - (const TSetBits &RawBits) const {
		return (*this - *(TSet*)&RawBits);
	}

	TSet& operator -= (const TSetBits &RawBits) {
		return (*this -= *(TSet*)&RawBits);
	}

	TSet operator * (const TSetBits &RawBits) const {
		return (*this * *(TSet*)&RawBits);
	}

	TSet& operator *= (const TSetBits &RawBits) {
		return (*this *= *(TSet*)&RawBits);
	}

	bool operator == (const TSetBits &RawBits) const {
		return (*this == *(TSet*)&RawBits);
	}

	bool operator != (const TSetBits &RawBits) const {
		return (*this != *(TSet*)&RawBits);
	}

	bool operator <= (const TSetBits &RawBits) const {
		return (*this <= *(TSet*)&RawBits);
	}

	bool operator >= (const TSetBits &RawBits) const {
		return (*this >= *(TSet*)&RawBits);
	}


	// Delphi / Binary Compatibility Functions ====================================================

	// Return the size of this set (in bytes) according to Delphi's binary format.
	// Note: If copying the actual set (in its unpacked format), use sizeof() for size calculations.
	int DelphiSize() const {
		return ((int)(High + 7) >> 3) - ((int)Low >> 3);
	}

	// Reads the set from memory according to Delphi's binary format.
	// Note: Reads the number of bytes returned by DelphiSize().
	void DelphiRead(void *pPtr) {
		Clear(); if (!pPtr) return;
		for (int o = (int)Low >> 3, i = 0; o <= (int)High >> 3;
			((char*)&bits)[o++] = ((char*)pPtr)[i++]);
	}

	// Write the set to memory according to Delphi's binary format.
	// Note: Writes the number of bytes returned by DelphiSize().
	void DelphiWrite(void *pPtr) const {
		if (!pPtr) return;
		for (int i = (int)Low >> 3, o = 0; i <= (int)High >> 3;
			((char*)pPtr)[o++] = ((char*)&bits)[i++]);
	}
};