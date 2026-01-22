-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE "ship_role" AS ENUM ('fabricator', 'harvester', 'hauler', 'interceptor', 'excavator');
CREATE TYPE "flight_mode" AS ENUM ('drift', 'stealth', 'burn', 'cruise');
CREATE TYPE "ship_status" AS ENUM ('in_transit', 'in_orbit', 'docked');
CREATE TABLE agents (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    "accountId" TEXT NOT NULL,
    symbol TEXT NOT NULL,
    headquarters TEXT NOT NULL,
    credits INT NOT NULL,
    "startingFaction" TEXT NOT NULL,
    "shipCount" INT NOT NULL
);
CREATE TABLE ships (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    symbol TEXT NOT NULL,
    "registrationName" TEXT NOT NULL,
    "registrationFactionSymbol" TEXT NOT NULL,
    "registrationRole" ship_role NOT NULL,
    "navSystemSymbol" TEXT NOT NULL,
    "navWaypointSymbol" TEXT NOT NULL,
    "navStatus" ship_status NOT NULL,
    "navFlightMode" flight_mode NOT NULL,
    "navRouteArrivalTime" TIMESTAMP WITH TIME ZONE NOT NULL,
    "navRouteDepartureTime" TIMESTAMP WITH TIME ZONE NOT NULL,
    "navRouteDestinationSymbol" TEXT NOT NULL,
    "navRouteDestinationType" TEXT NOT NULL,
    "navRouteDestinationX" INT NOT NULL,
    "navRouteDestinationY" INT NOT NULL,
    "navRouteOriginSymbol" TEXT NOT NULL,
    "navRouteOriginType" TEXT NOT NULL,
    "navRouteOriginX" TEXT NOT NULL,
    "navRouteOriginY" TEXT NOT NULL,
    "cargoCapacity" INT NOT NULL,
    "cargoUnits" INT NOT NULL,
    "cargoInventory" JSONB NOT NULL,
    "fuelCurrent" INT NOT NULL,
    "fuelCapacity" INT NOT NULL,
    "fuelConsumedAmount" INT NOT NULL,
    "fuelConsumedTimestamp" TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    "cooldownExpiration" TIMESTAMP WITH TIME ZONE NOT NULL,
    "cooldownTotalSeconds" INT NOT NULL,
    "agentId" UUID NOT NULL
);
ALTER TABLE ships ADD CONSTRAINT "ships_ref_agentId" FOREIGN KEY ("agentId") REFERENCES agents (id) ON DELETE NO ACTION;
CREATE TABLE agent_setup_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
