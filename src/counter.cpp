/*
 ApproxMC

 Copyright (c) 2019-2020, Mate Soos and Kuldeep S. Meel. All rights reserved
 Copyright (c) 2009-2018, Mate Soos. All rights reserved.
 Copyright (c) 2015, Supratik Chakraborty, Daniel J. Fremont,
 Kuldeep S. Meel, Sanjit A. Seshia, Moshe Y. Vardi
 Copyright (c) 2014, Supratik Chakraborty, Kuldeep S. Meel, Moshe Y. Vardi

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
 */

#include <ctime>
#include <cstring>
#include <errno.h>
#include <algorithm>
#include <string.h>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <map>
#include <set>
#include <fstream>
#include <sys/stat.h>
#include <string.h>
#include <list>
#include <array>
#include <cmath>
#include <complex>
//#include <coz.h>

#include "counter.h"
#include "time_mem.h"
#include "GitSHA1.h"
#ifdef CMS_LOCAL_BUILD
#include "arjun.h"
#else
#include <arjun/arjun.h>
#endif

#define verb_print(a, x) \
    do { if (conf.verb >= a) {std::cout << "c " << x << std::endl;} } while (0)

using std::cout;
using std::endl;
using std::map;
using std::ios;
using namespace AppMCInt;

Hash Counter::add_hash(uint32_t hash_index, SparseData& sparse_data)
{
    string randomBits;

    if (randfile.is_open()) {
        // set read position of random bits
        uint32_t pos_rand = base_rand + hash_index*(conf.sampling_set.size()+1);
        randfile.seekg(pos_rand, ios::beg);
     
        // read random bits
        randomBits.resize(conf.sampling_set.size()+1);
        randfile.read(&randomBits[0], conf.sampling_set.size()+1);
        if (randfile.gcount() != conf.sampling_set.size()+1) {
            cout << "[appmc] Cannot read " << conf.sampling_set.size()+1 << " random bits from file '" << conf.randfilename
                 << "'." << endl;
            exit(1);
        }
        // cout << "cccccccc " << "position: " << pos_rand << " random bits: " << randomBits << endl;
    } else {
        randomBits = gen_rnd_bits(conf.sampling_set.size(), hash_index, sparse_data);
    }

    vector<uint32_t> vars;
    for (uint32_t j = 0; j < conf.sampling_set.size(); j++) {
        if (randomBits[j] == '1') {
            vars.push_back(conf.sampling_set[j]);
        }
    }

    solver->new_var();
    const uint32_t act_var = solver->nVars()-1;
    bool rhs;
    if (randfile.is_open()) {
        rhs = randomBits.back() == '1';
    } else {
        rhs = gen_rhs();
    }
    Hash h(act_var, vars, rhs);

    vars.push_back(act_var);
    solver->add_xor_clause(vars, rhs);
    if (conf.verb_cls) {
        print_xor(vars, rhs);
    }

    return h;
}

void Counter::ban_one(const uint32_t act_var, const vector<lbool>& model)
{
    vector<Lit> lits;
    lits.push_back(Lit(act_var, false));
    for (const uint32_t var: conf.sampling_set) {
        lits.push_back(Lit(var, model[var] == l_True));
    }
    solver->add_clause(lits);
}

///adding banning clauses for repeating solutions
uint64_t Counter::add_glob_banning_cls(
    const HashesModels* hm
    , const uint32_t act_var
    , const uint32_t num_hashes)
{
    uint64_t repeat = 0;
    uint64_t checked = 0;

    if (hm != NULL) {
        assert(act_var != std::numeric_limits<uint32_t>::max());
        assert(num_hashes != std::numeric_limits<uint32_t>::max());

        for (uint32_t i = 0; i < hm->glob_model.size(); i++) {
            const SavedModel& sm = hm->glob_model[i];
            //Model was generated with 'sm.hash_num' active
            //We will have 'num_hashes' hashes active

            if (sm.hash_num >= num_hashes) {
                ban_one(act_var, sm.model);
                repeat++;
            } else {
                //Model has to fit all hashes
                checked++;
                bool ok = true;
                for(const auto& h: hm->hashes) {
                    //This hash is number: h.first
                    //Only has to match hashes below current need
                    //note that "h.first" is numbered from 0, so this is a "<" not "<="
                    if (h.first < num_hashes) {
                        ok &= check_model_against_hash(h.second, sm.model);
                        if (!ok) break;
                    }
                }
                if (ok) {
                    //cout << "Found repeat model, had to check " << checked << " hashes" << endl;
                    ban_one(act_var, sm.model);
                    repeat++;
                }
            }
        }
    }
    if (conf.verb) {
        cout << "c [appmc] repeat solutions: " << std::setw(6) << repeat
        << " checked: " << std::setw(6) << checked;
        if (hm) cout << " out of: " << std::setw(6) << hm->glob_model.size();
        cout << endl;
    }
    return repeat;
}

void Counter::dump_cnf_from_solver(const vector<Lit>& assumps)
{
    vector<vector<Lit>> cnf;
    solver->start_getting_small_clauses(
        std::numeric_limits<uint32_t>::max(),
        std::numeric_limits<uint32_t>::max(),
        false, true);

    uint32_t maxvars = 0;
    bool ret = true;
    vector<Lit> cl;
    while(ret) {
        ret = solver->get_next_small_clause(cl);
        if (!ret) {
            continue;
        }
        cnf.push_back(cl);
        for(const auto& l: cl) {
            maxvars = std::max<uint32_t>(l.var(), maxvars);
        }
    }

    for(const auto& l: assumps) {
        maxvars = std::max<uint32_t>(l.var(), maxvars);
    }

    solver->end_getting_small_clauses();

    std::stringstream ss;
    ss << "cnf_dump-" << cnf_dump_no << ".cnf";
    cnf_dump_no++;

    std::ofstream f;
    f.open(ss.str(), std::ios::out);
    f << "p cnf " << maxvars+1 << " " << cnf.size()+assumps.size() << "\n";
    for(const auto& l: assumps) {
        f << l << " 0\n";
    }

    for(const auto& c: cnf) {
        f << c << " 0\n";
    }
    f.close();
}

SolNum Counter::bounded_sol_count(
        uint32_t maxSolutions,
        const vector<Lit>* assumps,
        const uint32_t hashCount,
        HashesModels* hm
) {
    if (conf.verb) {
        cout << "c [appmc] "
        "[ " << std::setw(7) << std::setprecision(2) << std::fixed
        << (cpuTimeTotal()-startTime)
        << " ]"
        << " bounded_sol_count looking for " << std::setw(4) << maxSolutions << " solutions"
        << " -- hashes active: " << hashCount << endl;
    }

    //Set up things for adding clauses that can later be removed
    vector<Lit> new_assumps;
    if (assumps) {
        assert(assumps->size() == hashCount);
        new_assumps = *assumps;
    } else {
        assert(hashCount == 0);
    }
    solver->new_var();
    const uint32_t sol_ban_var = solver->nVars()-1;
    new_assumps.push_back(Lit(sol_ban_var, true));

    if (conf.simplify >= 2) {
        if (conf.verb >= 2) {
            cout << "c [appmc] inter-simplifying" << endl;
        }
        double myTime = cpuTime();
        solver->simplify(&new_assumps);
        total_inter_simp_time += cpuTime() - myTime;
        if (conf.verb >= 1) {
            cout << "c [appmc] inter-simp finished, total simp time: "
            << total_inter_simp_time << endl;
        }
    }

    const uint64_t repeat = (conf.reuse_models ? add_glob_banning_cls(hm, sol_ban_var, hashCount) : 0);
    uint64_t solutions = repeat;
    double last_found_time = cpuTimeTotal();
    vector<vector<lbool>> models;
    while (solutions < maxSolutions) {
        lbool ret = solver->solve(&new_assumps, true);
        assert(ret == l_False || ret == l_True);
        if (conf.dump_intermediary_cnf >= 2 && ret == l_True) {
            dump_cnf_from_solver(new_assumps);
        }
        if (conf.dump_intermediary_cnf >= 1 && ret == l_False) {
            dump_cnf_from_solver(new_assumps);
        }

        if (conf.verb >= 2) {
            cout << "c [appmc] bounded_sol_count ret: " << std::setw(7) << ret;
            if (ret == l_True) cout << " sol no.  " << std::setw(3) << solutions;
            else cout << " No more. " << std::setw(3) << "";
            cout << " T: "
            << std::setw(7) << std::setprecision(2) << std::fixed
            << (cpuTimeTotal()-startTime)
            << " -- hashes act: " << hashCount
            << " -- T since last: "
            << std::setw(7) << std::setprecision(2) << std::fixed
            << (cpuTimeTotal()-last_found_time) << endl;
            if (conf.verb >= 4) solver->print_stats();
            last_found_time = cpuTimeTotal();
        }
        if (ret != l_True) break;

        //Add solution to set
        solutions++;
        const vector<lbool> model = solver->get_model();
        check_model(model, hm, hashCount);
        models.push_back(model);

        //ban solution
        vector<Lit> lits;
        lits.push_back(Lit(sol_ban_var, false));
        for (const uint32_t var: conf.sampling_set) {
            assert(solver->get_model()[var] != l_Undef);
            lits.push_back(Lit(var, solver->get_model()[var] == l_True));
        }
        if (conf.verb_cls) {
            cout << "c [appmc] Adding banning clause: " << lits << endl;
        }
        solver->add_clause(lits);
    }

    //Save global models
    if (hm && (conf.reuse_models || !conf.certfilename.empty())) {
        for (const auto& model: models) {
            hm->glob_model.push_back(SavedModel(hashCount, model));
        }
    }

    //Remove solution banning
    vector<Lit> cl_that_removes;
    cl_that_removes.push_back(Lit(sol_ban_var, false));
    solver->add_clause(cl_that_removes);

    return SolNum(solutions, repeat);
}

void Counter::print_final_count_stats(ApproxMC::SolCount solCount)
{
    if (solCount.hashCount == 0 && solCount.cellSolCount == 0) {
        cout << "c [appmc] Formula was UNSAT " << endl;
    }

    if (conf.verb > 2) {
        solver->print_stats();
    }
}

ApproxMC::SolCount Counter::solve(Config _conf)
{
    conf = _conf;
    orig_num_vars = solver->nVars();
    startTime = cpuTimeTotal();

    openLogFile();
    openRandFile();
    openCertFile();
    randomEngine.seed(conf.seed);

    ApproxMC::SolCount solCount = count();
    print_final_count_stats(solCount);

    randfile.close();
    certfile.close();

    if (conf.verb) {
        cout << "c [appmc] ApproxMC T: "
        << (cpuTimeTotal() - startTime) << " s"
        << endl;
    }
    return solCount;
}

vector<Lit> Counter::set_num_hashes(
    uint32_t num_wanted,
    map<uint64_t, Hash>& hashes,
    SparseData& sparse_data
) {
    vector<Lit> assumps;
    for(uint32_t i = 0; i < num_wanted; i++) {
        if (hashes.find(i) != hashes.end()) {
            assumps.push_back(Lit(hashes[i].act_var, true));
        } else {
            Hash h = add_hash(i, sparse_data);
            assumps.push_back(Lit(h.act_var, true));
            hashes[i] = h;
        }
    }
    assert(num_wanted == assumps.size());

    return assumps;
}

void Counter::simplify()
{
    if (conf.verb >= 1) {
        cout << "c [appmc] simplifying" << endl;
    }

    solver->set_sls(1);
    solver->set_intree_probe(1);
    solver->set_full_bve_iter_ratio(conf.var_elim_ratio);
    solver->set_full_bve(1);
    solver->set_bva(1);
    solver->set_scc(1);

    solver->simplify();

    solver->set_sls(0);
    solver->set_full_bve(0);
    solver->set_bva(0);
}

//Set up probabilities, threshold and measurements
void Counter::set_up_probs_threshold_measurements(
    uint32_t& measurements, SparseData& sparse_data)
{
    int best_match = -1;
    bool using_sparse = false;
    double thresh_factor;

    if (conf.sparse) {
        best_match = find_best_sparse_match();
    }

    if (best_match != -1) {
        sparse_data = SparseData(best_match);
        thresh_factor = 1.1;
        using_sparse = true;
    } else {
        thresh_factor = 1.0;
    }

    threshold = int(
        1 +
        thresh_factor*
        9.84*
        (1.0+(1.0/conf.epsilon))*
        (1.0+(1.0/conf.epsilon))*
        (1.0+(conf.epsilon/(1.0+conf.epsilon)))
    );

    if (conf.verb) {
        cout
        << "c [appmc] threshold set to " << threshold
        << " sparse: " << (int)using_sparse
        << endl;
    }

    measurements = (int)std::ceil(std::log2(3.0/conf.delta)*17);
    for (int count = 0; count < 256; count++) {
        if (constants.iterationConfidences[count] >= 1 - conf.delta) {
            measurements = count*2+1;
            break;
        }
    }
}

bool Counter::find_one_solution()
{
    auto ret = solver->solve();
    return ret == l_True;
}

ApproxMC::SolCount Counter::count()
{
    int64_t hashCount = conf.start_iter;

    SparseData sparse_data(-1);
    HashesModels hm;
    uint32_t measurements;
    set_up_probs_threshold_measurements(measurements, sparse_data);

    verb_print(1, "[appmc] Starting at hash count: " << hashCount);

    int64_t mPrev = hashCount;
    numHashList.clear();
    numCountList.clear();

    //See Algorithm 1 in paper "Algorithmic Improvements in Approximate Counting
    //for Probabilistic Inference: From Linear to Logarithmic SAT Calls"
    //https://www.ijcai.org/Proceedings/16/Papers/503.pdf
    for (uint32_t j = 0; j < measurements; j++) {
        if (mPrev && mPrev == conf.sampling_set.size()) {
            mPrev--;
        }

        one_measurement_count(mPrev, j, sparse_data, &hm);

        // certification
        if (certfile.is_open()) {
            // initialization
            if(j == 0 && mPrev >= 1) {
                certfile << 0 << endl;
                certfile << threshold+1 << endl;
                assert(print_models(hm, 0) == threshold+1);
            }
            
            certfile << mPrev << endl; 
            if (mPrev >= 1) {
                certfile << threshold+1 << endl;
                assert(print_models(hm, mPrev-1) == threshold+1);
            }
            if (mPrev < conf.sampling_set.size()) {
                certfile << numCountList.back() << endl; 
                assert(print_models(hm, mPrev) == numCountList.back());
            }
        }

        if (mPrev == 0) {
            // Exact count, no need to measure multiple times.
            break;
        }
        sparse_data.next_index = 0;
        if (conf.simplify >= 1 && j+1 < measurements) simplify();
        hm.clear();
    }
    assert(numHashList.size() > 0 && "UNSAT should not be possible");

    return calc_est_count();
}

int Counter::print_models(HashesModels hm, int64_t hashCount)
{
    // number of solutions
    int count = 0;

    for (uint32_t i = 0; count < threshold+1 && i < hm.glob_model.size(); i++) {
        const SavedModel& sm = hm.glob_model[i];
        bool ok = true;
        if (conf.reuse_models) {
            if (sm.hash_num < hashCount) {
                for (const auto& h: hm.hashes) {
                    if (h.first < hashCount) {
                        ok &= check_model_against_hash(h.second, sm.model);
                        if (!ok) break;
                    }
                }
            }
        } else {
            if (sm.hash_num != hashCount) {
                ok = false;
            }
        }

        if (ok) {
            count++;
            // may not print the full model if Arjun simplifies the formula
            for (uint32_t var = 0; var < orig_num_vars; var++) {
                certfile << Lit(var, sm.model[var] == l_False) << ' ';
            }
            certfile << '0' << endl;
        }
    }

    return count;
}

ApproxMC::SolCount Counter::calc_est_count()
{
    ApproxMC::SolCount ret_count;
    if (numHashList.empty() || numCountList.empty()) {
        return ret_count;
    }

    const auto minHash = findMin(numHashList);
    auto cnt_it = numCountList.begin();
    for (auto hash_it = numHashList.begin()
        ; hash_it != numHashList.end() && cnt_it != numCountList.end()
        ; hash_it++, cnt_it++
    ) {
        *cnt_it *= pow(2, (*hash_it) - minHash);
    }
    ret_count.valid = true;
    ret_count.cellSolCount = findMedian(numCountList);
    ret_count.hashCount = minHash;

    return ret_count;
}

int Counter::find_best_sparse_match()
{
    for(int i = 0; i < (int)constants.index_var_maps.size(); i++) {
        if (constants.index_var_maps[i].vars_to_inclusive >= conf.sampling_set.size()) {
            if (conf.verb) {
                cout << "c [sparse] Using match: " << i
                << " sampling set size: " << conf.sampling_set.size()
                << " prev end inclusive is: " << (i == 0 ? -1 : (int)constants.index_var_maps[i-1].vars_to_inclusive)
                << " this end inclusive is: " << constants.index_var_maps[i].vars_to_inclusive
                << " next end inclusive is: " << ((i+1 < (int)constants.index_var_maps.size()) ? ((int)constants.index_var_maps[i+1].vars_to_inclusive) : -1)
                << " sampl size: " << conf.sampling_set.size()
                << endl;
            }

            return i;
        }
    }

    cout << "c [sparse] No match. Using default 0.5" << endl;
    return -1;
}

//See Algorithm 2+3 in paper "Algorithmic Improvements in Approximate Counting
//for Probabilistic Inference: From Linear to Logarithmic SAT Calls"
//https://www.ijcai.org/Proceedings/16/Papers/503.pdf
void Counter::one_measurement_count(
    int64_t& mPrev,
    const unsigned iter,
    SparseData sparse_data,
    HashesModels* hm)
{
    if (conf.sampling_set.empty()) {
        numHashList.push_back(0);
        numCountList.push_back(1);
        return;
    }

    //Tells the number of solutions found at hash number N
    //sols_for_hash[N] tells the number of solutions found when N hashes were added
    map<uint64_t,int64_t> sols_for_hash;

    //threshold_sols[hash_num]==1 tells us that at hash_num number of hashes
    //there were found to be FULL threshold number of solutions
    //threshold_sols[hash_num]==0 tells that there were less than threshold
    //number of solutions.
    //if it's not set, we have no clue.
    map<uint64_t,bool> threshold_sols;
    int64_t total_max_xors = conf.sampling_set.size();
    int64_t numExplored = 0;
    int64_t lowerFib = 0;
    int64_t upperFib = total_max_xors;
    threshold_sols[total_max_xors] = 0;
    sols_for_hash[total_max_xors] = 1;

    int64_t hashCount = mPrev;
    int64_t hashPrev = hashCount;

    base_rand = iter * conf.sampling_set.size() * (conf.sampling_set.size()+1);

    //We are doing a galloping search here (see our IJCAI-16 paper for more details).
    //lowerFib is referred to as loIndex and upperFib is referred to as hiIndex
    //The key idea is that we first do an exponential search and then do binary search
    //This is implemented by using two sentinels: lowerFib and upperFib. The correct answer
    // is always between lowFib and upperFib. We do exponential search until upperFib < lowerFib/2
    // Once upperFib < lowerFib/2; we do a binary search.
    while (numExplored < total_max_xors) {
        uint64_t cur_hash_count = hashCount;
        const vector<Lit> assumps = set_num_hashes(hashCount, hm->hashes, sparse_data);

        if (conf.verb) {
            cout << "c [appmc] "
            "[ " << std::setw(7) << std::setprecision(2) << std::fixed
            << (cpuTimeTotal()-startTime)
            << " ]"
            << " round: " << std::setw(2) << iter
            << " hashes: " << std::setw(6) << hashCount << endl;
        }
        double myTime = cpuTime();
        SolNum sols = bounded_sol_count(
            threshold + 1, //max no. solutions
            &assumps, //assumptions to use
            hashCount,
            hm
        );
        const uint64_t num_sols = std::min<uint64_t>(sols.solutions, threshold + 1);
        assert(num_sols <= threshold + 1);
        bool found_full = (num_sols == threshold + 1);
        write_log(
            false, //not sampling
            iter, hashCount, found_full, num_sols, sols.repeated,
            cpuTime() - myTime
        );

        if (num_sols < threshold + 1) {
            numExplored = lowerFib + total_max_xors - hashCount;

            //one less hash count had threshold solutions
            //this one has less than threshold
            //so this is the real deal!
            if (hashCount == 0 ||
                    (threshold_sols.find(hashCount-1) != threshold_sols.end()
                    && threshold_sols[hashCount-1] == 1)
            ) {
                numHashList.push_back(hashCount);
                numCountList.push_back(num_sols);
                mPrev = hashCount;
                return;
            }

            threshold_sols[hashCount] = 0;
            sols_for_hash[hashCount] = num_sols;
            if (iter > 0 &&
                std::abs(hashCount - mPrev) <= 2
            ) {
                //Doing linear, this is a re-count
                upperFib = hashCount;
                hashCount--;
            } else {
                if (hashPrev > hashCount) {
                    hashPrev = 0;
                }
                upperFib = hashCount;
                if (hashPrev > lowerFib) {
                    lowerFib = hashPrev;
                }

                hashCount = (upperFib+lowerFib)/2;
            }
        } else {
            assert(num_sols == threshold + 1);
            numExplored = hashCount + total_max_xors - upperFib;

            //success record for +1 hashcount exists and is 0
            //so one-above hashcount was below threshold, this is above
            //we have a winner -- the one above!
            if (threshold_sols.find(hashCount+1) != threshold_sols.end()
                && threshold_sols[hashCount+1] == 0
            ) {
                numHashList.push_back(hashCount+1);
                numCountList.push_back(sols_for_hash[hashCount+1]);
                mPrev = hashCount+1;
                return;
            }

            threshold_sols[hashCount] = 1;
            sols_for_hash[hashCount] = threshold+1;
            if (iter > 0
                && std::abs(hashCount - mPrev) < 2
            ) {
                //Doing linear, this is a re-count
                lowerFib = hashCount;
                hashCount++;
            } else if (lowerFib + (hashCount-lowerFib)*2 >= upperFib-1) {

                // Whenever the above condition is satisfied, we are in binary search mode
                lowerFib = hashCount;
                hashCount = (lowerFib+upperFib)/2;
            } else {

                // We are in exponential search mode.
                auto old_hashCount = hashCount;
                hashCount = lowerFib + (hashCount-lowerFib)*2;
                if (old_hashCount == hashCount) hashCount++;
            }
        }
        hashPrev = cur_hash_count;
    }
}
bool Counter::gen_rhs()
{
    std::uniform_int_distribution<uint32_t> dist{0, 1};
    bool rhs = dist(randomEngine);
    //cout << "rnd rhs:" << (int)rhs << endl;
    return rhs;
}

string Counter::gen_rnd_bits(
    const uint32_t size,
    // The name of parameter was changed to indicate that this is the index of hash function
    const uint32_t hash_index,
    SparseData& sparse_data)
{
    string randomBits;
    std::uniform_int_distribution<uint32_t> dist{0, 1000};
    uint32_t cutoff = 500;
    if (conf.sparse && sparse_data.table_no != -1) {
        //Do we need to update the probability?
        const auto& table = constants.index_var_maps[sparse_data.table_no];
        const auto next_var_index = table.index_var_map[sparse_data.next_index];
        if (hash_index >= next_var_index) {
            sparse_data.sparseprob = constants.probval[sparse_data.next_index];
            sparse_data.next_index = std::min<uint32_t>(
                sparse_data.next_index+1, table.index_var_map.size()-1);
        }
        assert(sparse_data.sparseprob <= 0.5);
        cutoff = std::ceil(1000.0*sparse_data.sparseprob);
        if (conf.verb > 3) {
            cout << "c [sparse] cutoff: " << cutoff
            << " table: " << sparse_data.table_no
            << " lookup index: " << sparse_data.next_index
            << " hash index: " << hash_index
            << endl;
        }
    }

    while (randomBits.size() < size) {
        bool val = dist(randomEngine) < cutoff;
        randomBits += '0' + val;
    }
    assert(randomBits.size() >= size);

    //cout << "rnd bits: " << randomBits << endl;
    return randomBits;
}

void Counter::print_xor(const vector<uint32_t>& vars, const uint32_t rhs)
{
    cout << "c [appmc] Added XOR ";
    for (size_t i = 0; i < vars.size(); i++) {
        cout << vars[i]+1;
        if (i < vars.size()-1) {
            cout << " + ";
        }
    }
    cout << " = " << (rhs ? "True" : "False") << endl;
}

template<class T>
inline T Counter::findMedian(vector<T>& numList)
{
    assert(!numList.empty());
    std::sort(numList.begin(), numList.end());
    size_t medIndex = numList.size() / 2;
    if (medIndex >= numList.size()) {
        return numList[numList.size() - 1];
    }
    return numList[medIndex];
}

template<class T>
inline T Counter::findMin(vector<T>& numList)
{
    T min = std::numeric_limits<T>::max();
    for (const auto a: numList) {
        if (a < min) {
            min = a;
        }
    }
    return min;
}

string scalmc_version_info()
{
    std::stringstream ss;
    ss << "c ApproxMC SHA revision " << AppMCInt::get_version_sha1() << endl;
    ss << "c ApproxMC version " << AppMCInt::get_version_tag() << endl;
    ss << "c ApproxMC compilation env " << AppMCInt::get_compilation_env() << endl;
    #ifdef __GNUC__
    ss << "c ApproxMC compiled with gcc version " << __VERSION__ << endl;
    #else
    ss << "c ApproxMC compiled with non-gcc compiler" << endl;
    #endif

    return ss.str();
}

string Counter::get_version_info() const
{
    string ret = ::scalmc_version_info();
    ret += solver->get_text_version_info();

    return ret;
}


void Counter::openLogFile()
{
    if (!conf.logfilename.empty()) {
        logfile.open(conf.logfilename.c_str());
        if (!logfile.is_open()) {
            cout << "[appmc] Cannot open Counter log file '" << conf.logfilename
                 << "' for writing." << endl;
            exit(1);
        }

        logfile << std::left
        << std::setw(5) << "sampl"
        << " " << std::setw(4) << "iter"
        << " " << std::setw(4) << "hash"
        << " " << std::setw(4) << "full"
        << " " << std::setw(4) << "sols"
        << " " << std::setw(4) << "rep"
        << " " << std::setw(7) << "T"
        << " " << std::setw(7) << "total T"
        << endl;

    }
}

void Counter::openRandFile()
{
    if (!conf.randfilename.empty()) {
        randfile.open(conf.randfilename.c_str());
        if (!randfile.is_open()) {
            cout << "[appmc] Cannot open Counter random bits file '" << conf.randfilename
                 << "' for reading." << endl;
            exit(1);
        }
    }
}

void Counter::openCertFile()
{
    if (!conf.certfilename.empty()) {
        certfile.open(conf.certfilename.c_str());
        if (!certfile.is_open()) {
            cout << "[appmc] Cannot open Counter certification file '" << conf.certfilename
                 << "' for writing." << endl;
            exit(1);
        }
    }
}

void Counter::write_log(
    bool sampling,
    int iter,
    uint32_t hashCount,
    int found_full,
    uint32_t num_sols,
    uint32_t repeat_sols,
    double used_time
)
{
    if (!conf.logfilename.empty()) {
        logfile
        << std::left
        << std::setw(5) << (int)sampling
        << " " << std::setw(4) << iter
        << " " << std::setw(4) << hashCount
        << " " << std::setw(4) << found_full
        << " " << std::setw(4) << num_sols
        << " " << std::setw(4) << repeat_sols
        << " " << std::setw(7) << std::fixed << std::setprecision(2) << used_time
        << " " << std::setw(7) << std::fixed << std::setprecision(2) << (cpuTimeTotal() - startTime)
        << endl;
    }
}

void Counter::check_model(
    const vector<lbool>& model,
    const HashesModels* const hm,
    const uint32_t hashCount
)
{
    for(uint32_t var: conf.sampling_set) assert(model[var] != l_Undef);
    if (!hm) return;

    for(const auto& h: hm->hashes) {
        //This hash is number: h.first
        //Only has to match hashes at & below
        //Notice that "h.first" is numbered from 0, so it's a "<" not "<="
        if (h.first < hashCount) {
            //cout << "Checking model against hash " << h.first << endl;
            assert(check_model_against_hash(h.second, model));
        }
    }
}

bool Counter::check_model_against_hash(const Hash& h, const vector<lbool>& model)
{
    bool rhs = false;
    for (auto const& var: h.hash_vars) {
        assert(model[var] != l_Undef);
        rhs ^= model[var] == l_True;
    }
    return rhs == h.rhs;
}
