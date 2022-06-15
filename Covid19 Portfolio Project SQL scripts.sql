
Select *
From [Project Portfolio]..[Covid Deaths]
where location is null
order by 3,4


--Select *
--From [Project Portfolio]..[Covid Vaccines]
--order by 3,4

--Select the data that we will be using

Select location, date, total_cases, new_cases, total_deaths, population
From [Project Portfolio]..[Covid Deaths]
order by 1,2

--Total cases vs Total Deaths

Select location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as PercentagePopulationInfected
From [Project Portfolio]..[Covid Deaths]
where continent is not null
--where location like '%Nigeria%'
order by 1,2

--Total cases vs Population

Select location, date,population, total_cases, (total_cases/population)*100 as PercentagePopulationInfected
From [Project Portfolio]..[Covid Deaths]
where continent is not null
--where location like '%Nigeria%'
order by 1,2

--Highest Infection Rate vs Population
Select location, population, max(total_cases) as HighestInfectionCount, (max(total_cases)/population)*100 as PercentagePopulationInfected
From [Project Portfolio]..[Covid Deaths]
where continent is not null
--where location like '%Nigeria%'
group by location, population
order by PercentagePopulationInfected desc

--Highest Death per Population
Select location, max(cast(total_deaths as int)) as TotalDeathCount
From [Project Portfolio]..[Covid Deaths]
where location is not null
--where location like '%Nigeria%'
group by location
order by TotalDeathCount desc

--By Continent
Select continent, max(cast(total_deaths as int)) as TotalDeathCount
From [Project Portfolio]..[Covid Deaths]
where location is null
--where location like '%Nigeria%'
group by continent
order by TotalDeathCount desc

--GLOBAL NUMBERS
Select date, sum(new_cases) as total_cases, sum(cast(new_deaths as int)) as total_deaths, sum(cast(new_deaths as int))/nullif(sum(new_cases),0)*100 as DeathPercentage
From [Project Portfolio]..[Covid Deaths]
where location is not null
--where location like '%Nigeria%'
group by date
order by 1,2


--Total Population vs Vaccination
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, sum(cast(vac.new_vaccinations as bigint)) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
From [Project Portfolio]..[Covid Deaths] dea
Join [Project Portfolio]..[Covid Vaccination] vac
on dea.location = vac.location
and dea.date = vac.date
order by 2,3

--USE CTE
With PopvsVac(Continent, Location, Date, Population, New_Vaccination, RollingPeopleVaccinated) as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, sum(cast(vac.new_vaccinations as bigint)) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
From [Project Portfolio]..[Covid Deaths] dea
Join [Project Portfolio]..[Covid Vaccination] vac
on dea.location = vac.location
and dea.date = vac.date
--order by 2,3
)
Select *, (RollingPeopleVaccinated/Population)*100
From PopvsVac

--TEMP TABLE
Drop Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_Vaccination numeric,
RollingPeopleVaccinated numeric
)
Insert into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, sum(cast(vac.new_vaccinations as bigint)) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
From [Project Portfolio]..[Covid Deaths] dea
Join [Project Portfolio]..[Covid Vaccination] vac
on dea.location = vac.location
and dea.date = vac.date
where dea.location is not null
--order by 2,3
Select *, (RollingPeopleVaccinated/Population)*100
From #PercentPopulationVaccinated
order by 2,3

--Creating view for visualization later
Print 'Percent of People Vaccinated'
GO

Create View PercentPopulationVaccinated as
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, sum(cast(vac.new_vaccinations as bigint)) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
From [Project Portfolio]..[Covid Deaths] dea
Join [Project Portfolio]..[Covid Vaccination] vac
on dea.location = vac.location
and dea.date = vac.date
where dea.location is not null
--order by 2,3
 Select * 
 from PercentPopulationVaccinated