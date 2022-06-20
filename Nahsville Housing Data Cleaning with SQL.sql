--Data Cleaning with SQL

Select *
From [Project Portfolio]..[Nahsville Housing]

--Change Date Format

Alter table [Nahsville Housing]
Alter column SaleDate Date;


--Populate Property Address

Select *
From [Project Portfolio]..[Nahsville Housing]
--where PropertyAddress is null
order by ParcelID


Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
From [Project Portfolio]..[Nahsville Housing] a
JOIN [Project Portfolio]..[Nahsville Housing] b
on a.ParcelID = b.ParcelID
and a.UniqueID <> b.UniqueID
where a.PropertyAddress is null

Update a
Set PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From [Project Portfolio]..[Nahsville Housing] a
JOIN [Project Portfolio]..[Nahsville Housing] b
on a.ParcelID = b.ParcelID
and a.UniqueID <> b.UniqueID
where a.PropertyAddress is null

--Breaking out Address into Individual Columns (Address, City, State)

Select PropertyAddress
From [Project Portfolio]..[Nahsville Housing]
--where PropertyAddress is null
--order by ParcelID

Select
Substring (PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1) as Address,
Substring (PropertyAddress, CHARINDEX(',', PropertyAddress) +1, Len(PropertyAddress)) as Address
From [Project Portfolio]..[Nahsville Housing]

Alter table [Nahsville Housing]
Add PropertySplitAddress nvarchar(255);

Update [Nahsville Housing]
Set PropertySplitAddress = Substring (PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1)

Alter table [Nahsville Housing]
Add PropertySplitCity nvarchar(255);

Update [Nahsville Housing]
Set PropertySplitCity = Substring (PropertyAddress, CHARINDEX(',', PropertyAddress) +1, Len(PropertyAddress))

Select *
From [Project Portfolio]..[Nahsville Housing]

Select OwnerAddress
From [Project Portfolio]..[Nahsville Housing]

Select
PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3),
PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2),
PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1)
From [Project Portfolio]..[Nahsville Housing]

Alter table [Nahsville Housing]
Add OwnerSplitAddress nvarchar(255);

Update [Nahsville Housing]
Set OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3)

Alter table [Nahsville Housing]
Add OwnerSplitCity nvarchar(255);

Update [Nahsville Housing]
Set OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2)

Alter table [Nahsville Housing]
Add OwnerSplitState nvarchar(255);

Update [Nahsville Housing]
Set OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1)

Select *
From [Project Portfolio]..[Nahsville Housing]


--Change Y and N to Yes and No in 'Sold As Vacant' Field

Select Distinct(SoldAsVacant), COUNT(SoldAsVacant)
From [Project Portfolio]..[Nahsville Housing]
Group by SoldAsVacant
Order by 2


Select SoldAsVacant,
Case When SoldAsVacant = 'Y' then 'Yes'
     When SoldAsVacant = 'N' then 'No'
	 Else SoldAsVacant
	 End
From [Project Portfolio]..[Nahsville Housing]


Update [Project Portfolio]..[Nahsville Housing]
Set SoldAsVacant = Case When SoldAsVacant = 'Y' then 'Yes'
     When SoldAsVacant = 'N' then 'No'
	 Else SoldAsVacant
	 End


--Remove Duplicates
WITH RowNumCTE AS (
Select *,
	ROW_NUMBER() Over (
	Partition by ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 Order by 
					UniqueID
					) row_num
From [Project Portfolio]..[Nahsville Housing]
--Order by ParcelID
)
Select * 
From RowNumCTE
where row_num > 1
Order by PropertyAddress


--Delete Unused Columns

Alter table [Project Portfolio]..[Nahsville Housing]
Drop Column OwnerAddress, PropertyAddress, TaxDistrict

Select*
From [Project Portfolio]..[Nahsville Housing]