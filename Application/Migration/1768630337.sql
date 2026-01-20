CREATE TYPE shiprole AS ENUM ('fabricator', 'harvester', 'hauler', 'interceptor', 'excavator');
CREATE TYPE flightmode AS ENUM ('drift', 'stealth', 'burn', 'cruise');
CREATE TYPE shipstatus AS ENUM ('in_transit', 'in_orbit', 'docked');
CREATE TABLE agents (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    accountid TEXT NOT NULL,
    symbol TEXT NOT NULL,
    headquarters TEXT NOT NULL,
    credits INT NOT NULL,
    startingfaction TEXT NOT NULL,
    shipcount INT NOT NULL
);
CREATE TABLE ships (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    symbol TEXT NOT NULL,
    registrationname TEXT NOT NULL,
    registrationfactionsymbol TEXT NOT NULL,
    registrationrole shiprole NOT NULL,
    navsystemsymbol TEXT NOT NULL,
    navwaypointsymbol TEXT NOT NULL,
    navstatus shipstatus NOT NULL,
    navflightmode flightmode NOT NULL,
    navroutearrivaltime TIMESTAMP WITH TIME ZONE NOT NULL,
    navroutedeparturetime TIMESTAMP WITH TIME ZONE NOT NULL,
    navroutedestinationsymbol TEXT NOT NULL,
    navroutedestinationtype TEXT NOT NULL,
    navroutedestinationx INT NOT NULL,
    navroutedestinationy INT NOT NULL,
    navrouteoriginsymbol TEXT NOT NULL,
    navrouteorigintype TEXT NOT NULL,
    navrouteoriginx TEXT NOT NULL,
    navrouteoriginy TEXT NOT NULL,
    cargocapacity INT NOT NULL,
    cargounits INT NOT NULL,
    cargoinventory JSONB NOT NULL,
    fuelcurrent INT NOT NULL,
    fuelcapacity INT NOT NULL,
    fuelconsumedamount INT NOT NULL,
    fuelconsumedtimestamp TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    cooldownexpiration TIMESTAMP WITH TIME ZONE NOT NULL,
    cooldowntotalseconds INT NOT NULL,
    agentid UUID NOT NULL
);
ALTER TABLE ships ADD CONSTRAINT "ships_ref_agentId" FOREIGN KEY (agentid) REFERENCES agents (id) ON DELETE NO ACTION;
CREATE TABLE agent_setup_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    status job_status DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT null,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    locked_by UUID DEFAULT null,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
